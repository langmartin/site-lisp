                                readme
                                ======

Author:  <lang.martin@gmail.com>
Date: 2009-08-10 10:56:49 


Table of Contents
=================
1 Documentation 
    1.1 make-tags-file.el 
        1.1.1 find-top-level isn't working correctly, at least on windows. 
2 Notes & Cheat Sheets 
    2.1 Org Mode 
        2.1.1 Timesheet reporting. 
    2.2 Dired 
        2.2.1 Filter dired by filename. 
        2.2.2 Mark by filename regexp. 
        2.2.3 Multi-file search & replace. 
    2.3 Eshell 
        2.3.1 Eshell redirect (not pipe) to a buffer. 
    2.4 Git 
        2.4.1 Search history. 
    2.5 MozREPL 
        2.5.1 Flush the cache from anywhere. 
        2.5.2 Enter the page context. 


1 Documentation 
~~~~~~~~~~~~~~~~
  This repository is my collection of elisp packages from all over the
  Internet, and also a collection of my own code, all mixed together.
  It is not well organized.

1.1 make-tags-file.el 
======================

1.1.1 TODO find-top-level isn't working correctly, at least on windows. 
------------------------------------------------------------------------

2 Notes & Cheat Sheets 
~~~~~~~~~~~~~~~~~~~~~~~

2.1 Org Mode 
=============

2.1.1 Timesheet reporting. 
---------------------------
    org-mode rocks the reporting.



2.2 Dired 
==========

2.2.1 Filter dired by filename. 
--------------------------------
    C-x d ~/dir/*.css

2.2.2 Mark by filename regexp. 
-------------------------------
    % m
    

2.2.3 Multi-file search & replace. 
-----------------------------------
    1. Open a dired buffer containing files containing a string
       M-x find-grep-dired RET <pattern>
    2. Mark all js files
       % m .js$ RET
    3. Run query-replace on marked files
       Q <pattern> RET <replacement>
    4. Save all modified files
       C-x s

2.3 Eshell 
===========

2.3.1 Eshell redirect (not pipe) to a buffer. 
----------------------------------------------
    diff > #<buffer *scratch*>

2.4 Git 
========

2.4.1 Search history. 
----------------------
    git log -S'pattern'

2.5 MozREPL 
============

2.5.1 Flush the cache from anywhere. 
-------------------------------------
    repl._creationContext.webdeveloper_clearCache()

2.5.2 Enter the page context. 
------------------------------
    repl.enter(content)
