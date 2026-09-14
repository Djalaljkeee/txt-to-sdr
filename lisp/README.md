# R90R.lsp

AutoLISP commands for AutoCAD:

* `R90R` — rotates **each** selected closed polyline by exactly **+45°**
  around **its own geometric center**. The angle is the single top-of-file
  constant `*R90R-DEG*` — set it to `90.0`, `30.0`, `-45.0` … as needed.
* `R90DIAG` — diagnostics: reports what is really in the selection
  (entity type, vertex count, closed/open, locked layer, block reference),
  i.e. *why* an object was not rotated.

Processed: closed `LWPOLYLINE` and closed 2D `POLYLINE` (heavy). A polyline
whose last vertex coincides with the first is treated as closed as well, even
when the `Closed` flag is not set. Skipped objects are counted and reported by
reason: open, locked layer, 3D/mesh polyline, not modifiable.

> A **square** rotated by a multiple of 90° looks exactly the same on screen —
> that is geometry, not a bug (`LIST` shows the vertex order has changed).
> At 45° the turn is plainly visible.

## Load

`APPLOAD` → select `R90R.lsp` → **Load** (optionally add it to *Startup Suite*).

## Run

1. Type `R90R`.
2. Select the polylines with a window / crossing / `ALL` — as many as you like.
3. Press `Enter`. The command reports how many objects were rotated and how
   many were skipped.

## How the center is found

The area centroid of each polygon is computed with Green's theorem over that
polyline's own vertices (DXF group 10):

```
A  = 1/2     * Σ ( xi·y(i+1) − x(i+1)·yi )
Cx = 1/(6·A) * Σ ( xi + x(i+1) ) · ( xi·y(i+1) − x(i+1)·yi )
Cy = 1/(6·A) * Σ ( yi + y(i+1) ) · ( xi·y(i+1) − x(i+1)·yi )
```

No bounding box is used, so the result is correct for polylines rotated at any
angle to the WCS. For a rectangle/square it is exactly the intersection of the
diagonals. Degenerate (zero area) shapes fall back to the average of vertices.

Vertices are rewritten in place with `entmod` around that center, so every
center stays in the same point and no object moves relative to any other.
Multiples of 90° use exact `cos/sin` values (0 / ±1) so those turns carry no
floating point dust at all.
