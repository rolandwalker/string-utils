[![Build Status](https://secure.travis-ci.org/rolandwalker/string-utils.png?branch=master)](http://travis-ci.org/rolandwalker/string-utils)

Overview
========

String-manipulation utilities for Emacs Lisp.

Quickstart
----------

```lisp
(require 'string-utils)
 
(string-utils-squeeze-filename (buffer-file-name (current-buffer)) 20)
 
(string-utils-stringify-anything (selected-frame))
 
(progn
  (message (string-utils-pad (buffer-name (current-buffer)) (window-width) 'right))
  (sit-for 1)
  (message (string-utils-pad (buffer-name (current-buffer)) (window-width) 'center))
  (sit-for 1)
  (message (string-utils-pad (buffer-name (current-buffer)) (window-width) 'left))
  (sit-for 1))
```

Explanation
-----------

String-utils is a collection of functions for string manipulation.
This library has no user-level interface; it is only useful
for programming in Emacs Lisp.

The following functions are provided:

	string-utils-stringify-anything
	string-utils-has-darkspace-p
	string-utils-has-whitespace-p
	string-utils-trim-whitespace
	string-utils-compress-whitespace
	string-utils-string-repeat
	string-utils-escape-double-quotes
	string-utils-quotemeta
	string-utils-pad
	string-utils-pad-list
	string-utils-propertize-fillin
	string-utils-squeeze-filename
	string-utils-squeeze-url
	string-utils-split

The most complex function is `string-utils-pad`, the docstring
for which is reproduced below:

(string-utils-pad STR-VAL WIDTH &optional MODE CHAR THROW-ERROR)

Pad STR-VAL to WIDTH.

Optional MODE defaults to `'right`, but may be `'left`, `'center`, or
an integer.

When MODE is `'left`, padding characters are prepended.  When MODE
is `'center`, padding characters are both appended and prepended so
that STR-VAL is centered within WIDTH.

When MODE is a positive integer, the behavior is fixed-position
padding.  Similar to `'center`, padding may be added on the right
and on the left.  Exactly MODE-many padding characters are
added on the left before padding to the full WIDTH on the right.
When MODE is a negative integer, the behavior is the same, except
that MODE fixes the right-side padding.

Optional CHAR sets the padding character (defaults to space).

Optional THROW-ERROR throws an error if the length of STR-VAL
already exceeds WIDTH, or if the fixed-position padding requested
would cause the result to exceed WIDTH.  When THROW-ERROR is not
set (the default), a best-attempt result is always returned.

Tabs are expanded to spaces according to the value of
`tab-width`.

Returns a padded copy of string STR-VAL.

Compatibility and Requirements
------------------------------

	GNU Emacs version 24.3-devel     : yes, at the time of writing
	GNU Emacs version 24.1 & 24.2    : yes
	GNU Emacs version 23.3           : yes
	GNU Emacs version 22.3 and lower : no

Uses if present: [list-utils.el](http://github.com/rolandwalker/list-utils)
