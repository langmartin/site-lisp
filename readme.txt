* Tips of the Day
  :PROPERTIES:
  :VISIBILITY: all
  :END:

** Dired Mode
*** Filter Dired by Filename
    Open a dired buffer containing css files in directory =dir=
    : C-x d ~/dir/*.css
    
*** Multi-File Search & Replace
    1. Open a dired buffer containing files containing a string
       : M-x find-grep-dired RET <pattern>
    2. Mark all js files
       : m .js$ RET
    3. Run query-replace on marked files
       : Q <pattern> RET <replacement>
    4. Save all modified files
       : C-x s

* COMMENT Org Mode
  These Org Mode notes are just for reference, the comments in this
  section are configuration flags.

  - S-<left> :: previous date
  - C-c . :: insert a date
  - C-c C-e :: export menu

  - C-c C-j :: jump around while searching.
  - M-<left> :: promote this heading
  - M-C-<left> :: promote this subtree

  - C-c C-u :: up to next headline.
  - C-<RET> :: make a new headline
  - C-c C-t :: cycle TODO status.

# Local Variables:
# mode:org
# End:
