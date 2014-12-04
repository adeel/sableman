sableman
========

Sableman is the Sable bundle manager.

What is Sable?
--------------
Sable is a semantic XML-based format for writing mathematical documents.

What is a Sable bundle?
-----------------------
A Sable bundle consists of a meta file together with documents.
Each document is roughly a list of paragraphs, of type "theorem", "proposition", "definition", etc.
Within such a paragraph it is possible to make citations to other paragraphs in the document, paragraphs in other documents in the bundle, and even paragraphs in other bundles.
For the latter purpose it is possible to specify bundle dependencies in the meta file, to be loaded from the local filesystem or the internet.
Mathematical formulas can be written in TeX syntax, inline within `$` signs, or in display mode with the `<f>` tag.

What is the difference between Sable and LaTeX?
-----------------------------------------------
Sable can be thought of as a semantic version of TeX or LaTeX, that also allows seamless cross-referencing between documents.
Sable is however not meant as a replacement of LaTeX; the latter is much more flexible and powerful for the purpose of working with individual documents.

What does Sableman do?
----------------------
Sableman helps you work with Sable bundles.
For example, it can download all the dependencies of a bundle, and export it to HTML or LaTeX format.

Development status
------------------
Sableman is functional, but not ready for public use yet.