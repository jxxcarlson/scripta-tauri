module Text exposing (about, l0Demo, microLaTeXDemo, testFile, xMarkdown, nada)

nada = """
| title
Oops

Sorry, no such document
"""

about = """


| title
Scripta Desktop

| contents


| section 1 -
What it is

With the Scripta you can create and edit documents in any one three 
markup languages:

| item
MicroLaTeX — similar to LaTeX

| item
XMarkdown — similar to Markdown, but you can do LaTeX-style mathematics

| item
L0 — a markup language with a Lisp-like syntax

Use the three buttons listed under [b Sample docs] to see
examples of documents written in each of these languages.

Scripta documents may be exported to standard LaTeX using the 
[b Export] button or to PDF using the  [b PDF]  button.

There is also a web version of Scripta Desktop at [link scripta.io https://scripta.io].


| section 1 -
How to use it

The best way ... TO BE CONTINUED

| section 1 -
Technical notes

Scripta Desktop is written in [link Elm https://elm-lang.org], a pure 
functional language designed for building web apps.  The 
[link tauri https://tauri.app] framework is used to build the desktop
version.  The software is open source; please see this [link github repo https://github.com/jxxcarlson/scripta-tauri].  

"""



microLaTeXDemoOLD =
    """
\\title{Demo (MicroLaTeX)}

| banner
\\link{Scripta.io https://scripta.io}

\\contents

\\section{Images}

\\image{https://see.news/wp-content/uploads/2020/12/UK_wildbirds-01-robin.jpg width:400}

\\section{Math}

Pythagoras says: $a^2 + b^2 = c^2$

From calculus:

$$
\\int_0^1 x^n dx = \\frac{1}{n+1}
$$

\\strong{Tip:} Click on a section title to go back to the table of contents.
"""


l0Demo =
    """
| title
Demo (L0)

| banner
[link Scripta.io https://scripta.io]

| contents

| section 1
Images

|| hide
[image https://nas-national-prod.s3.amazonaws.com/styles/hero_image/s3/web_h_apa_2016-a1_2474_8_cedar-waxwing_peter_brannon_kk_female.jpg?itok=VdeVVmGA]

[image https://www.birdsandblooms.com/wp-content/uploads/2018/10/BNBbyc18_patricia-warren.jpg width:400]


| section 1
Math

Pythagoras says: $a^2 + b^2 = c^2$

From calculus:

$$
\\int_0^1 x^n dx = \\frac{1}{n+1}
$$

[bold Tip:] Click on a section title to go back to the table of contents.

"""




xMarkdown =
    """
| title
XMarkdown Visual Check

| contents




@[tags check]


# Typography


Some *italic* and  **bold** text.  @[blue Blue stuff.]
(Cool, no!)


# Link

[New York Times](https://nytimes.com)

# Image



![Bird](https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcRhXplhW5-ydbNOJiJe1fp7oAM9xjRwX28ung&usqp=CAU)

# Mathematics

This is a formula: $a^2 + b^2 = c^2$.

## Displayed formula

$$
\\int_0^1 x^n dx = \\frac{1}{n+1}
$$


## Numbered formula

|| equation
\\int_0^1 x^n dx = \\frac{1}{n+1}

## Aligned equations


|| aligned
\\label{foo}
a &= b + 1 \\\\
c &= a^2 \\\\
 &= b^2 + 2b + 1 \\\\


## Theorems

| theorem
There are infinitely many primes $p \\equiv 1\\ mod\\ 4$. Isn't that nice?


# Code

Some inline code `a[0] := a[0] + 1`.  A block of code:

```
# multiplication table
  for x in range(1, 11):
      for y in range(1, 11):
          print('%d ** %d = %d' % (x, y, x**y)
```

Here is the source text:

|| code
```
# multiplication table
  for x in range(1, 11):
      for y in range(1, 11):
          print('%d ** %d = %d' % (x, y, x**y)
```

# Bulleted Lists

- One

- Two

  - Alpha

  - Beta

    - Ho ho ho!

    - Ha ha !


# Numbered Lists


. One

. Two

  . Alpha

  . Beta

    . Ho ho ho!

    . Ha ha!
"""


microLaTeXDemo = """
\\title{MicroLaTeX Demo}


\\contents

\\section{Mathematics}

Let $a$, $b$, and $c$ be the sides of a right triangle, where
$c$ is the hypotenuse.   Then $a^2 + b^2 = c^2$.

This will be on the test:

\\begin{equation}
\\label{on-the-test}
\\int_0^1 x^n dx = \\frac{1}{n+1}
\\end{equation}


\\begin{theorem}
  There are infinitely many prime numbers

  $$
  p \\equiv 1\\ mod\\ 4
  $$

  Isn't that nice?
\\end{theorem}


Aligned equations:

\\begin{aligned}
a &= x + y \\\\
b &= x - y \\\\
c &= ab \\\\
 & = (x + y)(x - y) \\\\
&= x^2 - y^2
\\end{aligned}



\\section{Code}


Use a `\\begin{code} ... \\end{code}`, as in the below. 


\\begin{code}
# multiplication table
  for x in range(1, 11):
      for y in range(1, 11):
          print('%d * %d = %d' % (x, y, x*y))
\\end{code}




\\section{Images}



Use the model `\\image{web-address}` to place an image.

\\image{https://news.wttw.com/sites/default/files/styles/full/public/field/image/CardinalSnowtlparadisPixabayCrop.jpg?itok=iyp0zGMz}

There are options, e.g., you can place a caption with the image:

\\image{https://news.wttw.com/sites/default/files/styles/full/public/field/image/CardinalSnowtlparadisPixabayCrop.jpg?itok=iyp0zGMz caption: Cardinal in Winter}

If an image is publically avaialable on the internet, you 
are all set.  If not, you can use a hosting service such as
\\link{imgbb.com https://imgbb.com/} to upload your images.  Like most such services, imgbb has a free tier.

"""

testFile1 =
    """

\\title{Wave Packets and the Dispersion Relation}

\\tags{jxxcarlson:wave-packets-dispersion, quantum-mechanics, system:startup, folder:krakow}


\\contents

| banner
\\link{Quantum Mechanics Notes https://scripta.io/s/jxxcarlson:quantum-mechanics-notes}

| setcounter 2

\\tags{system:startup jxxcarslon:wave-packets-dispersion}

\\image{https://psurl.s3.amazonaws.com/images/jc/sinc2-bcbf.png  caption:Wave packet width:300}

\\section{Introduction}

As we have seen with the sinc packet, wave packets can be localized in space.  A key feature of such packets is their \\term{group velocity} $v_g$.



This is the velocity which which the "body" of the wave packet travels.  Now a wave packet is synthesized by superposing many plane waves, so the natural question is how is the group velocity of the packet related to the phase velocities of its constituent plane waves.  We will answer this first in the simplest possible situation -- a superposition of two sine waves.  Next, we will reconsider the case of the sinc packet.  Finally, we will study a more realistic approximation to actual wave packets which gives insight into the manner and speed with which wave packets change shape as they evolve in time.  We end by applying this to an electron in a thought experiment in which it has been momentarily confned to an atom-size box -- about one Angstrom, or
$10^{-10} \\text{ meter}$.



\\section{A two-frequency packet: beats!!!!}

\\image{https://psurl.s3.amazonaws.com/images/jc/beats-eca1.png width:300 caption: Two-frequency beats}

Consider a wave
$\\psi = \\psi_1 + \\psi_2$ which is the sum of two terms with slightly different frequencies.  If the waves are sound waves, then then what one will hear is a pitch that corresponding to the average of the two frequencies modulated in such a way that the volume goes up and down at a frequency corresponding to their difference.

Let us analyze this phenomenon mathematically, setting



\\begin{equation}
\\psi_1(x,t)  = \\cos((k - \\Delta k/2)x - (\\omega - \\Delta \\omega/2)t)
\\end{equation}

and

\\begin{equation}
\\psi_2(x,t)  = \\cos((k + \\Delta k/2)x - (\\omega + \\Delta \\omega/2)t)
\\end{equation}

By the addition law for the sine, this can be rewritten as

\\begin{equation}
\\psi(x,t) = 2\\sin(kx - \\omega t)\\sin((\\Delta k)x - (\\Delta \\omega)t)
\\end{equation}


The resultant wave -- the sum -- consists of of a high-frequency sine wave oscillating according to the average of the component wave numbers and angular frequencies, modulated by a cosine factor that oscillates according to the difference of the wave numbers and the angular frequencies, respectively.  The velocity associated to the high frequency factor is

\\begin{equation}
v_{phase} = \\frac{\\omega}{k},
\\end{equation}

whereas the velocity associated with the low-frequency factor is

\\begin{equation}
v_{group} = \\frac{\\Delta \\omega}{\\Delta k}
\\end{equation}

This is the simplest situation in which one observes the phenomenon of the group velocity.  Take a look at this \\href{https://galileo.phys.virginia.edu/classes/109N/more_stuff/Applets/wavepacket/wavepacket.html}{animation}.


\\section{Step function approximation}

We will now find an an approximation to

\\begin{equation}
\\psi(x,t) = \\int_{-\\infty}^\\infty a(k) e^{i(kx - \\omega(k)t)} dk
\\end{equation}

under the assumption that $a(k)$ is nearly constant over an interval from $k_0 -\\Delta k/2$ to $k_0 + \\Delta k/2$ and that outside of that interval it approaches zero at a rapid rate.  In that case the Fourier integral is approximated by

\\begin{equation}
\\int_{k_0 - \\Delta k/2}^{k_0 + \\Delta k/2}  a(k_0)e^{i((k_0 + (k - k_0)x - (\\omega_0t + \\omega_0'(k - k_0)t))}dk,
\\end{equation}

where $\\omega_0 = \\omega(k_0)$ and $\\omega_0' = \\omega'(k_0)$.
This integral can be written as a product $F(x,t)S(x,t)$, where the first factor is "fast" and the second is "slow."  The fast factor is just

\\begin{equation}
F(x,t) = a(k_0)e^{ i(k_0x - \\omega(k_0)t) }
\\end{equation}

It travels with velocity $v_{phase} = \\omega(k_0)/k_0$.  Setting $k; = k- k_0$, the slow factor is

\\begin{equation}
S(x,t) = \\int_{-\\Delta k/2}^{\\Delta k/2} e^{ik'\\left(x - \\omega'(k_0)t\\right)} dk',
\\end{equation}

The slow factor be evaluated explicitly:

\\begin{equation}
I = \\int_{-\\Delta k/2}^{\\Delta k/2} e^{ik'u} dk' = \\frac{1}{iu} e^{ik'u}\\Big\\vert_{k' = - \\Delta k/2}^{k' = +\\Delta k/2}.
\\end{equation}

We find that

\\begin{equation}
I = \\Delta k\\thinspace \\text{sinc}\\frac{\\Delta k}{2}u
\\end{equation}

where $\\text{sinc } x = (\\sin x )/x$.  Thus the slow factor is

\\begin{equation}
S(x,t) = \\Delta k\\, \\text{sinc}(  (\\Delta k/2)(x - \\omega'(k_0)t)  )
\\end{equation}


Putting this all together, we have

\\begin{equation}
\\psi(x,t) \\sim a(k_0)\\Delta k_0\\, e^{i(k_0x - \\omega(k_0)t)}\\text{sinc}(  (\\Delta k/2)(x - \\omega'(k_0)t)  )
\\end{equation}

Thus the body of the sinc packet moves steadily to the right at velocity $v_{group} = \\omega'(k_0)$


\\section{Gaussian approximation}

The approximation used in the preceding section is good enough to capture and explain the group velocity of a wave packet.  However, it is not enough to explain how wave packets change shape as they evolve with time.  To understand this phenomenon, we begin with  an arbitrary packet

\\begin{equation}
\\psi(x,t) = \\int_{\\infty}^\\infty a(k) e^{i\\phi(k)}\\,dk,
\\end{equation}

where $\\phi(k) = kx - \\omega(k)t$.  We shall assume that the spectrum $a(k)$ is has a maximum at $k = k_0$ and decays fairly rapidly away from the maximum.  Thus we assume that the Gaussian function

\\begin{equation}
a(k) = e^{ -(k-k_0)^2/ 4(\\Delta k)^2}
\\end{equation}

is a good approximation.  To analyze the Fourier integral

\\begin{equation}
\\psi(x,t) = \\int_{-\\infty}^{\\infty} e^{ -(k-k_0)^2/ 4(\\Delta k)^2} e^{i(kx - \\omega(k) t)},
\\end{equation}

we expand $\\omega(k)$ in a Taylor series up to order two, so that

\\begin{equation}
\\phi(k) = k_0x + (k - k_0)x - \\omega_0t - \\frac{d\\omega}{dk}(k_0) t- \\frac{1}{2}\\frac{ d^2\\omega }{ dk^2 }(k_0)( k - k_0)^2 t
\\end{equation}

Writing $\\phi(k) = k_0x - \\omega_0t + \\phi_2(k,x,t)$, we find that

\\begin{equation}
\\psi(x,t) = e^{i(k_0x - \\omega_0 t)} \\int_{-\\infty}^{\\infty} e^{ -(k-k_0)^2/ 4(\\Delta k)^2} e^{i\\phi_2(k,x,t)}.
\\end{equation}

Make the change of variables $k - k_0 = 2\\Delta k u$, and write $\\phi_2(k,x,t) = Q(u,x,t)$, where $Q$ is a quadratic polynomial in $u$ of the form $au + b$. One finds that

\\begin{equation}
a = -(1 + 2i\\alpha t  (\\Delta k)^2),
\\end{equation}

where

\\begin{equation}
\\alpha = \\frac{ d^2\\omega }{ dk^2 }(k_0)
\\end{equation}

One also finds that

\\begin{equation}
b = 2i\\Delta k(x - v_g t),
\\end{equation}

where $v_g = d\\omega/dk$ is the group velocity.  The integral is a standard one, of the form

\\begin{equation}
\\int_{-\\infty}^\\infty e^{- au^2 + bu} = \\sqrt{\\frac{\\pi}{a}}\\; e^{ b^2/4a }.
\\end{equation}

Using this integral  formula and the reciprocity $\\Delta x\\Delta k = 1/2$, which we may take as a definition of $\\Delta x$, we find, after some algebra, that

\\begin{equation}
\\psi(x,t) \\sim A e^{-B} \\,e^{i(k_0 - \\omega_0t)}
,
\\end{equation}

where

\\begin{equation}
A = 2\\Delta k \\sqrt{\\frac{\\pi}{1 + 2i\\alpha \\Delta k^2 t}}
\\end{equation}

and

\\begin{equation}
B = \\frac{( x-v_gt )^2 (1 - 2i\\alpha \\Delta k^2 t)}{4\\sigma^2}
\\end{equation}

with

\\begin{equation}
\\sigma^2 = \\Delta x^2 + \\frac{\\alpha^2 t^2}{4 \\Delta x^2}
\\end{equation}

Look at the expression $B$. The first factor in the numerator controls the motion of motion of the packet and is what guides it to move with group velocity $v_g$.  The second factor is generally a small real term and a much larger imaginary one, and so only affects the phase.  The denominator controls the width of the packet, and as we can see, it increases with $t$ so long as $\\alpha$, the second derivative of $\\omega(k)$ at the center of the packet, is nonzero.

\\section{The electron!}

Let us apply what we have learned to an electron which has been confined to a box about the size of an atom, about $10^{-10}$ meters. That is, $\\Delta x \\sim 10^{-10}\\text{ m}$.  The extent of its wave packet will double when

\\begin{equation}
\\frac{\\alpha^2 t^2}{4 \\Delta x^2} \\sim \\Delta x^2,
\\end{equation}

that is, after a time

\\begin{equation}
t_{double} \\sim \\frac{\\Delta x^2}{\\alpha}
\\end{equation}

The dispersion relation for a free particle is

\\begin{equation}
\\omega(k) = \\hbar \\frac{k^2}{2m},
\\end{equation}

so that $\\alpha = \\hbar/m$.  Then

\\begin{equation}
t_{double} \\sim \\frac{m}{h}\\, \\Delta x^2 .
\\end{equation}

In the case of our electron, we find that $t_{double} \\sim 10^{-16}\\,\\text{sec}$.

\\section{ Code}

\\begin{code}
  # jupyter/python


  matplotlib inline

  # code for sinc(x)
  import numpy as np
  import matplotlib.pyplot as plt

  # sinc function
  x = np.arange(-30, 30, 0.1);
  y = np.sin(x)/x
  plt.plot(x, y)

  # beats
  x = np.arange(-50, 250, 0.1);
  y = np.cos(0.5*x) + np.sin(0.55*x)
  plt.plot(x, y)
\\end{code}



\\section{References}


\\bibitem{QM}
\\link{Quantum Mechanics for Engineers: Wave Packets https://www.eng.fsu.edu/~dommelen/quantum/style_a/packets.html}



\\bibitem{WP}
\\link{Wave Packets, Harvard Physics https://users.physics.harvard.edu/~schwartz/15cFiles/Lecture11-WavePackets.pdf}

\\bibitem{TE}
\\link{Time evolution in QM - MIT https//ocw.mit.edu/courses/nuclear-engineering/22-02-introduction-to-applied-nuclear-physics-spring-2012/lecture-notes/MIT22_02S12_lec_ch6.pdf}


"""


testFile =
    """
\\title{Test Math Macros (MicroLaTeX)}

\\tags{jxxcarlson:test-math-macros-microlatex, folder:scripta}

|| mathmacros
\\newcommand{\\cat}[1]{\\mathcal{#1}}
\\newcommand{\\foo}[1]{a_{#1}}
\\newcommand{\\op}[1]{\\mathop{\\text{#1}}}
\\newcommand{\\bool}{\\mathop{\\text{Bool}}}
\\newcommand{\\Type}{\\mathop{\\mathcal{U}}}


From old type theory notes


$$
true : \\bool \\qquad \\bool : \\Type
$$



$$
\\frac{\\Gamma \\vdash \\bool}{\\bool : \\Type}
$$

Macro expansion in superscript:

$$
a^{\\cat{C}^{op}}
$$

Macro expansion in nested superscript:

$$
a^{\\cat{C}^{\\cat{C}^{op}}}
$$


1: $\\red{\\cal{A}}$,
2: $\\cal{\\red{A}}$
3: $\\foo{x}$,
4: $\\foo{\\cal{C}}$
"""
