Cookie Cutter
=============
Cookie Cutter is a two-dimensional analog programming language.

A Cookie Cutter program, P, is a set of points in a two-dimensional plane, that
is applied to the data, D, which is a set of points in a two-dimensional plane.
The input is the initial data.

For purposes of exposition, points in the plane will be specified using
Cartesian coordinates.

Definitions
-----------
The data, D, can be divided into **edge points**, E, and **interior points**,
I.

A point, (x,y), is an **edge point** if there exists an (α,β) for which there
exists a δ > 0 for which for all 0 > ε > δ, (x+εα,y+εβ) ∉ D.

A point, (x,y), is an **interior point** if for all (α,β) there exists a δ > 0
for which for all 0 > ε > δ, (x+εα,y+εβ) ∈ D.

The data, D, can also be divided into 0 or more **subregions**.  Any point in a
**subregion** S can be connected to any other point in S by a continuous path
that consists only of points in I.  Subregions may overlap, but the overlap can
only contain points in E.

The program P can be mapped to P(a,b,s) by scaling and translation, where a
point (x,y) is mapped to (sx+a,sy+b).

A mapping P(a,b,s) is **contained by** a subregion S if all points in P(a,b,s)
are in S.

A mapping P(a,b,s) is **interior to** S if P(a,b,s) is **contained by** S and
all points in P(a,b,s) are **interior points**.

A mapping P(a,b,s) is an **executable mapping** in S if it is **contained by**
S but not **interior to** S and for all (α,β,σ) ≠ (0,0,0), where σ ≥ 0, there
exists a δ > 0 for which for all 0 > ε > δ, P(a+εα,b+εβ,s+εσ) either is
**interior to** S or is not **contained by** S.

Execution
---------
At each step, if D has no **subregions** or if no **subregion** of D has an
**executable mapping**, execution terminates.

If there is a single **executable mapping**, P(a,b,s), the output for the
step is s,{(a,b)} and the points in P(a,b,s) are either removed from D or
become **edge points** in D.  The new **edge points** do not necessarily
border points not in D, but do serve to divide D into new **subregions**.

If there are multiple **executable mappings**, then if there is a single
**executable mapping** P(a,b,s) with the greatest s, the output is s,{(a,b)},
and P(a,b,s) divides D into new **subregions**.

If there are multiple **executable mappings** P(a<sub>i</sub>,b<sub>i</sub>,s)
with the greatest s, and there is no point that is in more than one of the
**executable mappings**, the output is s,{(a<sub>i</sub>,b<sub>i</sub>)},
and all the **executable mappings** with the greatest s divide D into new
**subregions**.

If there are multiple **executable mappings** P(a<sub>i</sub>,b<sub>i</sub>,s)
with the greatest s, and points that are in more than one **executable
mapping** form a 2-dimensional area, execution terminates.

If there are multiple **executable mappings** P(a<sub>i</sub>,b<sub>i</sub>,s)
with the greatest s, and points that are in more than one **executable
mapping** form either 0-dimensional points or 1-dimensional curves, the output
is s,{(a<sub>i</sub>,b<sub>i</sub>)}, and all the **executable mappings** with
the greatest s divide D into new **subregions**.

Examples
--------
D is the square, -1 ≤ x ≤ 1 and -1 ≤ y ≤ 1.
P is the same square, -1 ≤ x ≤ 1 and -1 ≤ y ≤ 1.
The first step is 1,{(0,0)}, then execution terminates because D is empty.

D is the square, -1 ≤ x ≤ 1 and -1 ≤ y ≤ 1.
P is the square, -1 < x < 1 and -1 < y < 1.
The first step is 1,{(0,0)}, then execution terminates because there are no
**subregions**.

D is the square, -1 < x < 1 and -1 < y < 1.
P is the square, -1 ≤ x ≤ 1 and -1 ≤ y ≤ 1.
Execution terminates immediately because there are no **executable mappings**.

D is the square, -1 ≤ x ≤ 1 and -1 ≤ y ≤ 1.
P is the line segment, -1 ≤ x ≤ 1 and y = 0.
Execution terminates immediately because there are no **executable mappings**.

D is the square, -1 ≤ x ≤ 1 and -1 ≤ y ≤ 1.
P is the cross, -1 ≤ x ≤ 1, y = 0, and x = 0, -1 ≤ y ≤ 1.
The first step is 1,{(0,0)}, then
the second step is ½,{(-½,-½),(½,-½),(-½,½),(½,½)},
etc.

D is the rectangle, -1 ≤ x ≤ 2 and -1 ≤ y ≤ 1.
P is the cross, -1 ≤ x ≤ 1, y = 0, and x = 0, -1 ≤ y ≤ 1.
Execution terminates immediately because there are no **executable mappings**.

D is the square, -1 ≤ x ≤ 1 and -1 ≤ y ≤ 1.
P is the annulus, γ ≤ x<sup>2</sup> + y<sup>2</sup> ≤ 1, where 0 < γ < 1.
The first step is 2,{(0,0)}.  Subsequent steps depend on γ.  Larger values
of γ means the second step is 2γ,{(0,0)}, smaller values of γ means there
are 4 **executable mappings** in the second step, and, for one value of γ,
there are 5 **executable mappings** in the second step.

Additional Note
---------------
I don't know what would happen if P or D were fractal.  If D is fractal, there
can be points in D that are neither **edge points** nor **interior points**.
My guess is that, in most cases, execution would terminate.
