+++
title = "Interpreting Implication"
date = 2020-08-23
[taxonomies]
tags = ["logic"]
[extra]
headline = "Material implication is really about monotonic reasoning"
math = true
comment = true
+++

> Disclaimer: This post was translated into English by an AI model. It may contain mistakes or awkward wording.

The first difficulty in propositional logic is how to interpret implication. Logic textbooks give many explanations, but few give a more direct way to understand it. Here I try to derive the truth table for implication directly from the perspective of monotonic reasoning.

<!--more-->

# The Problem

This post assumes the reader has basic knowledge of propositional logic: what propositional calculus is and what its basic properties are. On that basis, we discuss the following question:

> Why is an implication \\(p \rightarrow q\\) in propositional logic true when its antecedent[^1] is false[^2]?

Or:

> Why is the standard interpretation of implication \\(\rightarrow\\) the following?
>
> | p | q | p→q |
> |---|---|-----|
> | 0 | 0 | 1   |
> | 0 | 1 | 1   |
> | 1 | 0 | 0   |
> | 1 | 1 | 1   |

Informally:

> Why is it true that if cats can fly, then pigs can fly?

Of course, informal discussion always has weaknesses, and we will soon see where they are.

Because we are discussing propositional logic, this post does not distinguish between "true" and "provable".

# Common Explanations and Their Weaknesses

## Converting to an Equivalent Proposition

One common explanation is to say that \\(p \rightarrow q\\) is equivalent to \\(\lnot p \lor q\\), or that "if p then q" has no counterexample.

This helps one memorize the truth table, but it does not help one understand why the definition is what it is.

## A Set-theoretic Explanation

For readers familiar with set theory, the most natural thought may be:

> For each atomic proposition \\(p\\), when \\(p\\) is true, there is a set \\(P\\) whose elements \\(x \in P\\) make \\(p\\) true.
>
> If \\(p \rightarrow q\\), then \\(P\\) is a subset of \\(Q\\).

Then naturally:

> If \\(p\\) is false, its corresponding set is \\(P = \emptyset\\), and the empty set is a subset of every set.

This explanation has serious flaws:

1. An alert reader will notice that we have left propositional logic, or zero-order logic, and entered first-order logic. First-order logic differs greatly from zero-order logic; for example, propositional logic has a decision procedure. We do not want to introduce first-order concepts when explaining zero-order logic.
2. The relationship between propositions and sets is not obvious. It is hard to say whether "the empty set is a subset of every set" causes "false implies everything", or whether "false implies everything" causes "the empty set is a subset of every set".

Compared with the two more direct explanations below, this explanation looks more technical, but is actually not very helpful for understanding.

## Axiom Schemas of Propositional Logic

Propositional logic has the axiom schema:

> \\(p \rightarrow (q \rightarrow p)\\)

This is also called affirmation of the consequent in the original post's terminology. Soundness requires this axiom schema to be true for any propositions \\(p\\) and \\(q\\). Enumerating truth tables gives only two solutions: the trivial solution and the standard interpretation of implication. In other words, this axiom uniquely determines the nontrivial interpretation of implication.

This explanation also has weaknesses:

1. It is hard to say whether the standard interpretation of implication led to the choice of this axiom schema, or the other way around.
2. Even if the axiom schema comes first logically, why design such an axiom schema?

Despite these weaknesses, if we follow the second question, we will eventually reach the answer proposed in this post.

## The "If-Then" Explanation

The most natural translation of \\(p \rightarrow q\\) is: if p, then q.

Faced with a proposition such as:

> If 0 = 1, then pigs can fly.

we must admit that the "if-then" proposition itself is fine, because there is no situation in which the condition holds. If the condition cannot hold, the consequence cannot be realized either. So it expresses a reasonable inference.

If you are willing to transcend ordinary human language, perhaps this is a sufficient explanation. But ordinary linguistic intuition still finds it strange. This is unsurprising, because natural language never corresponds directly to "implication". If anything, it is closer to an inference rule. In ordinary language, a meaningful "if-then" usually requires that the premise might hold under some circumstances, or at least that one can imagine it holding.

Compare the following:

1. If I can get good grades, then if I study hard, I can get good grades.
2. If there is a theorem \\(P \rightarrow Q\\), and there is also a theorem \\(P\\), then there is a theorem \\(Q\\).

Which one is closer to ordinary-language "if-then"? Hint: a normal person should choose the second.

Of course, natural-language "if-then" has broader uses. It can even express something like:

> If I had bought a house a few years earlier, I would not be under so much pressure now.

That is a perfectly reasonable expression.

It is precisely this "if-then" explanation that reveals the difference between implication and ordinary language when the premise is impossible. So this explanation does not really explain anything; it merely forces us to revise our native-language intuition. But it is also an important clue that will lead us to the final answer.

# Pieces of the Argument

Agenda:

1. Propositional logic is an artificial construction, and we designed it with goals and expectations.
2. Our expectations for propositional logic imply that propositional logic should be monotonic.
3. Monotonicity derives the axiom schema above.
4. That axiom schema uniquely determines the nontrivial interpretation of implication, as discussed earlier.

## Expectations for Propositional Logic

Mathematical logic began with Leibniz's "fantasy": to grasp human thought through mathematics, so that computation alone could determine whether any proposition is true or false. We know mathematicians well enough to know that they especially like simple structures and dislike unnecessary axioms. Propositional logic is such a simple system: first-order logic with quantification removed. First-order logic can describe a great deal of mathematics. Before describing all human thought, mathematicians first worried about whether their own reasoning could go wrong: do mathematical proofs, exchanged in language, really achieve consensus? Do they contain fatal flaws that make all mathematicians mistaken?

Mathematicians thought: since proofs and propositions are written in language, why not design a simpler language and study whether reasoning in that language goes wrong? The operations in the meta-system should be as few as possible.

1. **Goal**: use only certain symbols and their calculus to capture part of the structure of truth.
2. **Soundness**: conclusions obtained through symbolic calculation are always true, regardless of interpretation.[^3]
3. **Completeness**: all truths can be confirmed by this calculus.

Let us provisionally accept that our designed language has these properties, and then discuss the result.

At this point we can already design symbols such as \\(\land\\) and \\(\lor\\):

1. If we have \\(p\\) and \\(q\\), then we have \\(p \land q\\).
2. If we have \\(p \land q\\), then we have \\(p\\) and also \\(q\\).
3. If we have \\(p\\), then for any proposition \\(q\\), we have \\(p \lor q\\).
4. And so on.

Again, the "if-then" here belongs to the metalanguage describing inference rules, not to implication inside the system.

## Monotonic Logic

For use in mathematics, one basic requirement is that conclusions already derived and confirmed remain true forever. This is very natural in mathematics; otherwise, as we add conditions, conclusions would decrease. Such things happen all the time in natural language. For example:

> I always go to the playground after class.

But suppose we add a premise:

> Yesterday I did not do my homework, so after class the teacher called me to the office to make it up.

The first sentence is now invalid.[^4]

Mathematics, however, must remain consistent at all times. For example, if there is a set \\(S\\) such that \\(\forall x \in S, x > 100\\), then \\(S\\) is already fixed.

From the requirement of monotonic logic, we can directly derive:

> Informally: under the premise that p holds, any proposition q, when added as a premise, still gives p.

This conclusion cannot be expressed as an inference rule, because it does not generate a new proposition.

How can we express this property inside the formal language? If the language cannot express it, then it has failed to capture the property of "reasoning" that interests us.

## Implication Is Also a Proposition

By freeing reasoning from interpretation, we can grasp reasoning itself more directly. But so far, the system itself cannot reason internally, because it cannot describe dependency relations among propositions. So we introduce implication with the intuition of "premise-conclusion":

1. Introduce the logical connective \\(\rightarrow\\), constructing propositions of the form \\(p \rightarrow q\\).
2. **Modus ponens**: if we have \\(p \rightarrow q\\) and \\(p\\), then we have \\(q\\).

After this operation, the system can express "premise-conclusion" internally. Another important result is that many inference rules can be demoted to propositions inside the system. In fact, propositional logic only needs modus ponens as its single inference rule. Let us translate the informal monotonicity statement:

> "With proposition q as a premise, we have p" is expressed as \\(q \rightarrow p\\).
>
> "Under the premise p, with proposition q as a premise, we have p" is expressed as \\(p \rightarrow (q \rightarrow p)\\).

Thus we obtain the axiom schema. As mentioned above, this axiom uniquely determines the nontrivial interpretation of implication.

# Further Questions and Answers

> This seems a little contradictory, because implication was introduced using the intuition of "if ... then". If it does not match our intuition, does that not mean the design is flawed?

Not really. We find that implication and ordinary language differ only when the antecedent is false, and monotonic reasoning is extremely useful in mathematics, so we tolerate this inconsistency.

> Implication is just a logical connective, no different from \\(\land\\) or \\(\lor\\). It does not mean p and q have any essential relationship.

Exactly. That does not prevent us from investigating why it is defined this way, nor does it change the fact that our intuition for it is "if-then".

> I still do not understand!

Restating the core idea: if P is true, then P remains true under any circumstances.

The most likely reason for confusion is treating "if-then" as causality. In natural language, "if it rains, then the ground is wet". But the "if-then" of implication is a static descriptive relation. The antecedent and consequent have no temporal order in logic; it is merely the construction of a new proposition from two propositions.

# Conclusion

This was originally an answer I wrote on Zhihu. It was later deleted and abridged when I closed that account. Now... never mind.

Actually, an old classmate was studying logic and had exactly the same confusion, so I rewrote the article.

This post attributes the interpretation of material implication to the monotonicity of classical logic. I first arrived at the idea independently, after reading quite a bit of related material and never seeing anyone state it this way. Later I found that the conclusion is already common knowledge in logic.[^5] The only thing I still find puzzling is why this explanation has not become more widespread.

---

[^1]: An implication is a proposition of the form \\(p \rightarrow q\\). Here \\(p\\) is called the antecedent, \\(q\\) the consequent, and \\(\rightarrow\\) the implication connective.

[^2]: All occurrences of "true" in this post refer to the standard interpretation. The standard interpretation of implication is called material implication.

[^3]: This property is also called logical validity.

[^4]: Stronger logics can handle this situation.

[^5]: From "[Two Examples of Non-monotonic Logic in Scientific Research](http://blog.sciencenet.cn/home.php?mod=space&uid=40049&do=blog&id=257970)": "Non-monotonic logic rejects the monotonicity of implication."

