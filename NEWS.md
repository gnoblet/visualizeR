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
