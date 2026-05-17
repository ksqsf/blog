+++
title = "Notes on Tinkering with Rime Double Pinyin"
date = 2023-06-01T17:07:59+08:00
updated = 2023-12-26T22:06:16+08:00
[taxonomies]
tags = ["ime", "rime", "double_pinyin"]
[extra]
headline = "Using two translators together without hurting myself"
math = true
comment = true
+++

> Disclaimer: This post was translated into English by an AI model. It may contain mistakes or awkward wording.

Since last June, when I began learning shape-based input methods again, I somehow wandered into the bottomless world of Chinese input methods. After becoming familiar with various input schemes, I also started tinkering with schemes myself. What surprised me during this process was that Rime can completely realize my idea of "double-pinyin plus auxiliary shape codes with sentence input", and the cost is quite low. To spread the Rime gospel, and to promote my idea, this post describes in detail how I implemented my input scheme in Rime and explains some of the difficulties and solutions.

<!-- more -->

## Double Pinyin with Auxiliary Codes and Sentence Input

Double pinyin is an efficient phonetic input scheme. Each pinyin syllable without tone is represented by two letters: the first letter represents the initial, and the second represents the final, including medial sounds. The mapping from letters to initials and finals differs. For example, in the Ziranma double-pinyin scheme, `U` as the first letter represents `sh`, and `D` as the second letter represents `uang`; therefore `UD` represents `shuang`, while `UU` represents `shu`, not the meaningless `shsh`. Double pinyin can represent all Mandarin syllables with two letters, reducing code length compared with full pinyin.

However, pure phonetic input has an essential weakness: Chinese has too many homophones. Even if a long pinyin syllable is compressed into two letters, duplicate codes are not reduced. Therefore some schemes introduce shape codes to distinguish homophones. Ziranma was one of the earlier schemes of this kind. In Ziranma, each character has two phonetic letters and two shape-code letters. The first shape code represents the radical, and the second represents the part left after removing the radical.[^1] For example, the double-pinyin code of "码" is `MA`; its radical is "石" (`U`), and the remaining part is "马" (`M`), so its full code is `MAUM`. The character "码" has many homophones such as "吗", "麻", "骂", and "马", but adding shape information greatly reduces collisions. Such schemes are often colloquially called "double pinyin plus shape".

Unfortunately, existing implementations of double-pinyin plus shape schemes all have small problems.

**1. Table input.** Characters and words are encoded into a table, with fixed maximum code length, and input is performed like Wubi by table lookup. Typical examples include Xiaohe and Ziranma's table-input mode. Their maximum code length is four. Because the maximum length is fixed, the encoding space is limited: four codes over 26 letters give only \\(26^1 + 26^2 + 26^3 + 26^4 = 475254\\) possibilities. Chinese pronunciations are unevenly distributed, and characters and words share the same code space, so only a small dictionary can be used. The advantages are that input-method requirements are low, it can be mounted onto almost any Wubi-style input method, and accuracy is high: once skilled, one can type blindly without a candidate window.

**2. Character/word auxiliary input.** Examples include later Ziranma, Luoge, and Shouxin. These input methods treat auxiliary codes as real members of the input method, supporting dynamic filtering and word creation. However, they often imitate Ziranma by allowing auxiliary codes only at the end of a word. For example, the double-pinyin code of "它们" is `TAMF`; to exclude "他们" and "她们", you can add an auxiliary code, but only at the end: `TAMFb`. This avoids jumping back to add an auxiliary code after a previous character during word-by-word input, but the disadvantages are obvious: when entering multiple words at once, you can only filter the last word; and only two auxiliary-code letters can be entered, which may not be enough for long words. This kind of scheme is fully compatible with intelligent pinyin sentence input and is much easier to learn than table input.

**3. Mounting onto intelligent pinyin.** Although the previous category has advantages, it requires the input method itself to support double-pinyin auxiliary codes specially. Modern intelligent pinyin input methods, represented by Sogou, already have high accuracy. If we mount a table from the first category onto them as custom phrases, we can input single characters and some common words accurately while keeping sentence input unaffected. But once we enter sentence-input mode, auxiliary codes can no longer be used.

All three methods have some determinism: part of the code space maps uniquely. For example, typing `d` plus space always gives "的". Deterministic codes, once mastered, reduce mental burden and improve typing speed.

A few months ago, while making a phonetic-shape scheme for traditional inherited characters, I was inspired by the mounting approach and began wondering whether one could implement "double pinyin plus auxiliary codes with sentence input". This method is like double-pinyin sentence input, but allows auxiliary codes to be added after any character to help the pinyin engine choose characters. For example, "世界" and "视界" have identical pronunciation but very different meanings. What if we could add auxiliary codes directly during input?[^2]

![](./horizon.avif)

I tried this idea with [Rime](https://rime.im/). To my surprise, it is not only [possible](https://github.com/ksqsf/rime-moran/), but not expensive to implement.

Below I describe how to implement it:

1. use the script translator to implement non-shift[^3] double-pinyin sentence input with auxiliary codes;
2. use the table translator for short codes and short words;
3. use a Lua plugin to resolve conflicts between the table translator and the script translator so word creation works normally.

I solved some other problems as well, but will not discuss them here due to space.

## Sentence Auxiliary Input: Script Translator

Rime supports pinyin input. What many people may not know is that Rime supports pinyin through a fairly general algorithm; it does not special-case pinyin much. The core idea here is: **if we regard double-pinyin plus shape codes as a kind of pinyin, then Rime can automatically support sentence input with auxiliary codes**.

We will create a Rime schema called `lizi` ("example").

### A Full-code Table for Single Characters with Auxiliary Codes

In 2013, Fuzhen introduced [an auxiliary-code method](https://tieba.baidu.com/p/2094178562) on the Rime forum. The basic idea is to append shape auxiliary codes to each character's phonetic code. We use exactly the same method to make a full-code table for single characters.

```yaml
# File: lizi.dict.yaml
# encoding: utf-8
---
name: lizi
version: "1"
sort: by_weight
use_preset_vocabulary: false
...
时	ui;oc	999000
间	jm;mo	999000
世	ui;cv	999000
界	jx;tj	999000
事	ui;aa	999000
件	jm;rn	990000
视	ui;ju	990000
```

Then, following schemes such as Luna Pinyin, write a basic pinyin schema:

```yaml
# File: lizi.schema.yaml
# encoding: utf-8
schema:
  schema_id: lizi
  name: 例子
  version: "0.1"
  description:

switches:
  - name: ascii_mode
    reset: 0
    states: [ 中文, 西文 ]
  - name: full_shape
    states: [ 半角, 全角 ]
  - name: simplification
    reset: 1
    states: [ 漢字, 汉字 ]
  - name: ascii_punct
    states: [ 。，, ．， ]

engine:
  processors:
    - ascii_composer
    - recognizer
    - key_binder
    - speller
    - punctuator
    - selector
    - navigator
    - express_editor
  segmentors:
    - ascii_segmentor
    - matcher
    - abc_segmentor
    - punct_segmentor
    - fallback_segmentor
  translators:
    - punct_translator
    - script_translator
  filters:
    - simplifier
    - uniquifier

speller:
  alphabet: abcdefghijklmnopqrstuvwxyz;
  delimiter: " '"

translator:
  dictionary: lizi
```

After deployment, choose the "example" scheme. Typing codes such as `u` or `j` will show candidates.

### Spelling Algebra

We have implemented a double-pinyin scheme with auxiliary codes, but testing quickly reveals that Rime cannot support abbreviated spelling. If you type `uijm`, Rime cannot automatically split it into `ui jm`. Also, the current scheme still needs `;` to introduce auxiliary codes. To solve these problems, we use [spelling algebra](https://github.com/rime/home/wiki/SpellingAlgebra) to generate real input codes for each character.

Specifically, we want a character's actual input code to be `yyxx`, where `yy` is phonetic and `xx` is shape, and we want `y`, `yy`, `yyx`, and `yyxx` all to be able to input the character. Modify `speller` as follows:

```yaml
# lizi.schema.yaml
speller:
  alphabet: abcdefghijklmnopqrstuvwxyz  # ; can be removed
  delimiter: " '"
  algebra:
    - derive/^(\w*);(\w)(\w)$/$1$2$3/
    - derive/^(\w*);(\w)(\w)$/$1$2/
    - derive/^(\w*);(\w)(\w)$/$1/
```

The official documentation is a little convoluted, but spelling algebra simply derives new codes from existing ones. For `ui;oc`, the second rule matches and produces `uio`: group 1 is `ui`, group 2 is `o`, and group 3 is `c`. Thus the user can type `uio` to find "时".

Through spelling algebra, we implement abbreviation and no longer need `;`. At this point, sentence input with double-pinyin auxiliary codes is essentially complete. Yes, it is that simple.

### Intelligent Word Creation

The advantage of treating double-pinyin plus auxiliary codes as pinyin syllables is that Rime can automatically perform intelligent word creation, which is a feature of pinyin input itself. We do not need extra adaptation.

For example, with the scheme above, typing "ui jx" (shi jie) gives "事界" as the first candidate because we currently have no dictionary and Rime does not know the best choice is "世界". But when we type "uic jx", Rime automatically produces "世界". After committing it, Rime learns the word, and future input of "ui jx" can produce it.

![](./world.avif)

## Short Codes and Short Words: Table Translator

Using the script translator for double-pinyin sentence input with auxiliary codes is already good, but our requirements are usually higher. Like pinyin input, this method has **prefix monotonicity**, a term I invented for this phenomenon: "的" has very high frequency, so when typing `d`, we already know it is "的"; if we continue typing the full code `de`, it must still be "的". More precisely, a proper prefix `X` of a full code `XY` is not treated as a separate code. The character mapped by `X` must be the same as that mapped by `XY`; when the user sees character C after typing `X`, continuing C's code will not make C become another character. I call this a kind of monotonicity.

But this wastes encoding space. Originally, `d` and `de` could map to different characters, such as `d` -> "的" and `de` -> "得". Now, since `d` is not an independent code, `d` and `de` must be the same character.

Besides wasting code space, this monotonicity also harms determinism. Because a proper prefix is not itself a code, it is affected by usage frequency. Readers may occasionally find that the first candidate for `d` is not "的" but another character.

Therefore we want to break this monotonicity. This is not a new idea; short codes in shape-based input methods are exactly this. The essence of a short code is to manually specify mappings for some full-code prefixes, increasing code space and reducing collisions.

To fix code mappings in Rime, we need the table translator. Introduce a new dictionary:

```yaml
# guding.dict.yaml
# encoding: utf-8
---
name: guding
version: "1"
sort: original
...

时	u
间	j
世	uic
界	jx
事	ui
件	jm
视	uij
为什么	w
```

The "fixed" dictionary fixes mappings for some codes.

To let the `lizi` scheme reference this dictionary, create a dummy schema:

```yaml
# guding.schema.yaml
schema:
  schema_id: guding
  name: 工具方案-勿用
translator:
  dictionary: guding
  enable_user_dict: false
```

Then add the dependency to `lizi`:

```yaml
# lizi.schema.yaml
schema:
  schema_id: lizi
  name: 例子
  version: "0.1"
  description:
  dependencies:
    - guding
```

Finally, add a table translator. The table translator is used by shape-based methods such as Wubi. Its working principle is simple: look up the table and output the result, which is ideal for short-code input.

```yaml
# lizi.schema.yaml
engine:
  # ...
  translators:
    - punct_translator
    - reverse_lookup_translator
    - table_translator@guding    # new
    - script_translator
  # ...

guding:
  dictionary: guding
  initial_quality: 5
  enable_user_dict: false
  enable_sentence: false
  enable_encoder: false
  encode_commit_history: false
```

Here `initial_quality` can be set to a relatively large number so that table output appears before script-translator output. In addition, `table_translator` should appear before `script_translator`: user input passes through translators in order, and we want the table translator to process input first. If the table has no match, then input enters the script translator.

At this point, all major design work for the scheme is complete.

## Word Creation: A Top-level Translator

Although the main body of the scheme is complete, introducing the table translator creates a major problem for word creation.

Suppose we want to create the word "世间". We type `ui jm`; for the sake of explanation, do not use auxiliary codes here. Then we manually choose "世", and then choose "间". In principle, we expect the input method to create the word "世间". However, no matter how you try, if you input the word this way, it cannot be created.

This is because after we choose "世", the remaining input state is only `jm`, and `jm` enters the table translator, so "间" is output by the table translator. The script translator never sees that we input "间", and of course it cannot create the word "世间".

A complete solution may be complicated. My current solution is: after the user has already selected part of the characters, temporarily disable the table translator.

This sounds simple, but librime cannot directly implement the idea. I implemented it by writing a custom top-level translator using librime's Lua plugin support. The top-level translator calls the table and script translators in order depending on the current state. According to the [documentation](https://github.com/hchunhui/librime-lua/wiki/Scripting), this can be done with `Component`.

```lua
-- lua/top_translator.lua
local top = {}
local fixed = nil
local smart = nil

function top.init(env)
   -- Create translator components for later calls
   fixed = Component.Translator(env.engine, "", "table_translator@guding")
   smart = Component.Translator(env.engine, "", "script_translator@zhengju")
end

function top.fini(env)
end

function top.func(input, seg, env)
   if (env.engine.context.input == input) then
      local fixed_res = fixed:query(input, seg)
      for cand in fixed_res:iter() do
         yield(cand)
      end
   end

   local smart_res = smart:query(input, seg)
   for cand in smart_res:iter() do
      yield(cand)
   end
end

return top
```

Here `(env.engine.context.input == input)` checks whether the user has already selected characters. Then connect this translator to the scheme:

```yaml
# lizi.schema.yaml
engine:
  # ...
  translators:
    - punct_translator
    - reverse_lookup_translator
    - lua_translator@*top_translator
    # remove the previous table_translator and script_translator
  # ...

# Rename the previous translator: to zhengju:, matching the reference in top_translator.lua
zhengju:
  dictionary: lizi
  prism: lizi

guding:
  dictionary: guding
  initial_quality: 5
  enable_user_dict: false
  enable_sentence: false
  enable_encoder: false
  encode_commit_history: false
```

## Other Problems and More Ideas

Although we implemented a fairly complex double-pinyin auxiliary-code sentence input method with only a small amount of code, practical use still has many small problems. I already have preliminary solutions for some of them; others are still being explored.

* Character-word code collisions. The full code of "半" is `bjbf`, which collides with "版本". How should these be separated?
* Character and word frequency. We are building the character and word dictionaries entirely from scratch, so we cannot use the pronunciation and frequency information from the standard essay corpus.
* Polyphonic characters and tolerance codes. Suppose a character has \\(n\\) pronunciations and \\(m\\) auxiliary codes, one of which is correct and the other \\(m-1\\) are tolerance codes. Then the character has \\(nm\\) codes. If a word contains multiple such characters, the word will have especially many encodings.

In addition, after learning what Lua plugins can do, I think Rime can actually implement true "intelligent auxiliary codes": instead of appending auxiliary codes directly after phonetic codes in the dictionary, query and filter dynamically at runtime. This requires manually writing a new translator. The engineering effort is unknown, but I think it is worth trying.

<aside>
<p>This idea has now been implemented! <a href="https://github.com/HowcanoeWang/rime-lua-aux-code/">rime-lua-aux-code</a> implements dynamic indirect auxiliary codes through <code>lua-filter</code>. I separately implemented <a href="https://github.com/ksqsf/rime-moran/blob/4a927132f7968be0ce3c3282540c148b9645834a/lua/moran_aux_translator.lua">dynamic direct auxiliary codes</a>.</p>
</aside>

# Conclusion

This post described in some detail the core design and implementation of the "[modified Ziranma](https://github.com/ksqsf/rime-moran/)" scheme. As far as I know, Moran is the first double-pinyin plus auxiliary-code scheme that supports sentence input. Its implementation depends on the solid foundation laid by Rime. I hope this post can (1) encourage Rime users with ideas to participate in scheme design, (2) encourage scheme designers with ideas to try Rime, and (3) provide solutions that help people facing similar problems.

---

[^1]: In general, after removing the radical, the remaining part is a complete readable character. Sometimes the removed part is not itself a character, or is too obscure; Ziranma then takes the largest character component as close to the end as possible.

[^2]: This method attaches auxiliary codes after "characters" rather than after "words", which some people may find less convenient than character/word auxiliary input. However, it strengthens memory of single-character codes, which is advantageous for future single-character input or creating new words.

[^3]: No extra character is needed to enter an "auxiliary-code input state"; you directly type auxiliary-code letters. For example, "码" can be entered directly as `MAUM`, without pressing Tab, `;`, or another non-letter key after `MA`.
