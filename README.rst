emacs-fireplace
================

.. image:: https://api.travis-ci.org/johanvts/emacs-fireplace.svg
   :target: https://travis-ci.org/johanvts/emacs-fireplace
.. image:: https://coveralls.io/repos/johanvts/emacs-fireplace/badge.svg
   :target: https://coveralls.io/r/johanvts/emacs-fireplace
.. image:: http://melpa.org/packages/fireplace-badge.svg
   :target: http://melpa.org/#/fireplace
.. image:: http://stable.melpa.org/packages/fireplace-badge.svg
   :target: http://stable.melpa.org/#/fireplace
.. image:: https://img.shields.io/github/tag/johanvts/emacs-fireplace.svg
   :target: https://github.com/johanvts/emacs-fireplace/tags
.. image:: http://img.shields.io/:license-gpl3-blue.svg
   :target: http://www.gnu.org/licenses/gpl-3.0.html

.. |br| raw:: html

.. image:: https://raw.github.com/johanvts/emacs-fireplace/master/img/fireplace.gif

About
-----
A cozy fireplace for emacs.

Installation
------------

Download ``fireplace.el`` and place it somewhere in your ``.emacs.d`` directory, say in ``.emacs.d/fireplace/``.
Compile the file using ``M-x byte-compile-file``.
Put ``(load "~/.emacs.d/fireplace/fireplace")`` in your init file (``.emacs``).
Note that there is a significant performance difference between the compiled and non-compiled fireplace.


You can start the fire using ``M-x fireplace``.
The fireplace will try to fill the current window with a new buffer.
To put the fire out use ``M-x fireplace-off`` or hotkey ``q``.


========================= ================================
Key bind                  Function
========================= ================================
``C-+``                   Move fire up
``C--``                   Move fire down
``C-*``                   Toggle smoke
``q``                     Turn off fire
========================= ================================

Use ``M-x customize-group RET fireplace RET`` to view and change the user defined variables and read their documentation.

Acknowledgment
--------------

A big thanks to Dan Torop for his `emacs animation guide
<http://dantorop.info/project/emacs-animation/>`_ and Vasilij Schneidermann for the `xbm-life <https://github.com/wasamasa/xbm-life>`_ package.
The code relies heavily on these two sources.

Contribution
------------

Please feel free to do whatever you want with this code.
Ideas and pull requests are very welcome. I can be reached through johanvts@gmail.com
or on twitter @johanvts.


To-Do
-----
- Better status bar
- Custom faces support
