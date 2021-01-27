---
title: The rank-nullity theorem
tags: math, linear-algebra, abstract-algebra
---

This post is about the what the rank-nullity theorem, a rather
basic and fundamental result in linear algebra, really means.

First, I will state a result about vector spaces from which 
one could prove the rank-nullity theorem:

__Theorem 1__: every short exact sequence of vector spaces splits.

This looks nothing like the rank-nullity theorem! But first we can look at
how this implies rank-nullity. A _short exact sequence_ is just a sequence
$$0 \rightarrow U \xrightarrow{f} V \xrightarrow{g} W \rightarrow 0$$
of vector spaces (or $R$-modules, which are exactly the same as vector spaces 
except that they are defined over a base ring $R$ that is not necessarily a field), 
where $f$ is injective, $g$ is surjective, and $\mathrm{im} \ f = \ker g$ (hence the name "exact").

This sequence is said to be _split_ when $V \cong U \oplus W$. A short exact sequence is 
interesting, because it tells us how $V$ is built from $U$ and $W$: if the 
exact sequence is split, then we have the simplest building method (namely, by direct sum), which 
make things much easier. Another way to think about this is by looking at the splitting lemma:

__Lemma__ (splitting lemma): for a short exact sequence 
$$0 \rightarrow L \xrightarrow{f} M \xrightarrow{g} N \rightarrow 0$$
of $R$-modules, where $R$ is any ring, the following are equivalent:
    
  * $f$ is left-invertible, i.e. there is a morphism $h : B \rightarrow A$ such 
    that $h \circ f = \mathrm{id}_A$;
  * $g$ is right-invertible;
  * the sequence is split, i.e. $M \cong L \oplus N$.
  
I won't repeat the proof here, because it could be found in any graduate algebra text. On a 
side note, this facts actually holds over any abelian category, not just $\mathbf{Mod} R$.

The fact that every short exact sequence of vector spaces splits is rooted in a much simpler 
fact: any vector space has a basis. We can use this fact to prove Theorem 1:

__Proof of Theorem 1__: let $\{w_i\}$ be a basis of $W$. Since $g$ is necessarily surjective,.
we can find a set $\{v_i\} \subseteq V$ such that $f(v_i) = w_i$. Then we define a 
function $h : W \rightarrow V$ by setting $h(w_i) = v_i$. This is well-defined because a linear 
map is determined by its action on basis elements. $h$ is a right inverse to $g$, so by the splitting 
lemma this short exact sequence is split.

__Proof of rank-nullity__: at this point, the rank-nullity theorem is almost trivial. Simply consider 
the short exact sequence:
$$0 \rightarrow \ker f \hookrightarrow V \twoheadrightarrow \mathrm{im} \ f \rightarrow 0$$
and observe that, since the sequence is split, $V \cong \ker f \oplus \mathrm{im} \ f$. Taking dimension on
both sides yields the rank-nullity theorem.

In fact, we have proven a statement that is more general than the rank-nullity theorem! When $V$ is infinite-dimensional, 
the rank-nullity theorem fails, but $V \cong \ker f \oplus \mathrm{im} \ f$ still holds.

This, again, tells us one important fact about vector spaces: that linear maps contain all the information we need to 
study a space. This is, again, due to the fact that every vector space has a basis, i.e. a generating set that is linearly 
independent! This is what makes vector spaces so much simpler than general modules.

On the other hand, the very familiar first isomorphism theorem also implies rank-nullity:

__Theorem 2__ (the first isomorphism theorem): let $M, N$ be modules over a base ring, 
and $f : M \rightarrow N$ a linear map. Then $\mathrm{im} \ f \cong M/\ker f$.

This is slightly weaker than Theorem 1, but enough to prove rank-nullity:

__Alternate proof__: simply take the dimension on both sides in the isomorphism given by the first isomorphism
theorem.

This reiterates the essence of rank-nullity: the rank-nullity theorem is just the first isomorphism theorem in 
disguise, telling us that a linear map divides the information about a vector space into two complementary parts!
