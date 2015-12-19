emacs-fireplace
================
.. image:: https://raw.github.com/johanvts/emacs-fireplace/master/img/fireplace.gif

About
-----
A cozy fireplace for emacs.

Installation
------------

Download ''fireplace.el'' and place it somewhere in your ''.emacs.d'' directory, say in ''.emacs.d/fireplace/''.
Compile the file using ''M-x byte-compile-file''.
Put ''(load "~./.emacs.d/fireplace/fireplace") in your init file ('.emacs').
Note that there is a significant performance difference between the compiled and non-compiled fireplace.

You can start the fire using ''M-x fireplace''.
The fireplace will try to fill the current window with a new buffer.
To put the fire out use ''M-x fireplace-off''.


========================= ================================
Key bind                  Function
========================= ================================
``C-+``                   Move fire up
``C--``                   Move fire down
``C-s``                   Toggle smoke
========================= ================================

All variables starting with ''fireplace-'' can be customized. Use ''C-h v'' to  read their documentation. 

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
- Get the package into MELPA.
- Group up variables for easier customization.
- Better status bar
- Support using ''kill-buffer'' to turn the fire off.
