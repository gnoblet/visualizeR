# visualizeR 0.4.9000

* Breaking changes: remove dependency to `ggblanket`.
* Full rewrite of `theme_reach()`.
* `bar_reach` is now `bar()` and theming is passed through argument `theme` for which default is `theme_reach()`.
* `point_reach` is now `point()` and theming is passed through argument `theme` for which default is `theme_reach()`.


# visualizeR 0.3.9000

* Breaking changes: update to `ggblanket` v1.6.1.
* Add plotting functions for indicator maps.

# visualizeR 0.2.9000

* Breaking changes: almost all functions got refinements, and there are new functions, typically `hbar()` becomes `bar_reach()` and `point_reach()` is added.
* Following `theme_reach()` is now used by all plotting functions.
* Add README.md.

---

# visualizeR 0.1.7.9000

* Fixed some color palettes.

---

# visualizeR 0.1.6.9000

* IMPACT colors and palettes are added: function `cols_impact()` `pal_impact()`.
* Color palettes from REACH are added (2 to 7 continuous palettes) ; see updated `cols_reach()` and `pal_reach()`.

---

# visualizeR 0.1.5.9000

* Move from `simplevis` to successor `ggblanket`.

---

# visualizeR 0.1.4.9000

* `hbar()` gains a new boolean argument `reverse` to pass to `pal_reach()` or `pal_agora()`, indicating if the color palette should be reversed or not.

---

# visualizeR 0.1.3.9000

* Small change to `hbar()`: removes error arg within `simplevis::gg_hbar()` call.

---

# visualizeR 0.1.2.9000

* There was a duplicate `scale_color()` function, which should have been and is now `scale_fill()`

--- 

# visualizeR 0.1.1.9000

* Added two horizontal bar functions: `hbar()`, `hbar_percent()` (#3)
* Added some internals to check for missing columns and bad arguments (#3)
* Modified some `theme_reach()` documentation
* Add `buffer_bbox()` function to produce a buffered bbox, e.g. for use with `tmap`

--- 

# visualizeR 0.1.0

* Added a `NEWS.md` file to track changes to the package
* Initiate repo
