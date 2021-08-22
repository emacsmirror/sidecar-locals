
##############
SideCar Locals
##############

This is a global minor mode that provides a flexible alternative to Emacs built-in ``dir-locals`` functionality.


Motivation
==========

The motivation for this project was to be able to conveniently configure projects.

The main differences between this package and ``dir-locals`` are:

Code Execution
   Unlike dir-locals which requires ``(eval ...)`` blocks which need to be explicitly trusted.

   This package runs the emacs-lisp directly,
   since trust is manged at the path level instead of individual variables.

   While both methods are valid, executing code directly is more convenient when the code contains project-level logic
   (where every change to the code doesn't require re-trusting, continually growing the ``custom.el`` file).

Out of Source Configuration
   Storing personal configuration in ``dir-locals.el`` is not always appropriate for larger projects.

   Or alternatively having local ``dir-locals.el`` files scattered around the source directory,
   which can't be easily versioned and can complicate operations such as bisecting or switching branches
   directories won't be properly removed if they contain files not tracked by the version control.

   Similar to out-of-source builds, this package supports out-of-source locals so your source repository
   can be kept pristine and your configuration can be stored and versioned separately.


An example of what the directory layout might look like where ``/src/my-project`` and ``/src/other-project/`` are two
version controlled repositories:

.. code-block::

   /src/
   /src/my-project/
   /src/other-project/
   /src/.sidecar-locals/
   /src/.sidecar-locals/my-project(c-mode).el
   /src/.sidecar-locals/other-project(python-mode).el

In this example the ``/src/`` directory would have to be added to ``sidecar-locals-paths-allow``.


Usage
=====

- Install the package and enable the global mode with ``(sidecar-locals-mode)``.
- Create a ``.sidecar-locals`` directory in the projects root (this may be above the version control root)
  for out-of-source locals.

- Add this path to ``sidecar-locals-paths-allow`` (including the trailing slash).

- Open a file where you would like locals to apply and run ``sidecar-locals-report``.

  This will print a list of files which would be used if found for the current buffer.

- Create one of more of these files, then reload the current buffer for the locals to be detected and applied.


Customization
-------------

``sidecar-locals-ignore-modes`` (nil)
   Major modes that disable ``sidecar-mode``.

``sidecar-locals-paths-allow`` (nil)
   A list of strings, each string should be a directory that contains a ``.sidecar-locals`` directory.

   - The trailing slash is needed on each path.
   - A directory and all it's subdirectories can be matched by adding a ``*`` to the end.

   Example:

   .. code-block:: elisp

      (setq sidecar-locals-paths-allow
            (list "/my/repositories/project/"
                  "/my/personal/projects/*"))

``sidecar-locals-paths-deny`` (nil)
   Paths to disallow, setting this suppresses warnings when untrusted paths are found.
   This uses the same format as ``sidecar-locals-paths-allow``.

``sidecar-locals-ignore-buffer`` (nil)
   When non-nil, ``sidecar-locals`` wont be used for this buffer,
   when this is a callable, it will be called with the current buffer,
   a nil return value is used to disable.

``sidecar-locals-dir-name`` (``.sidecar-locals``)
   The subdirectory to search for when detecting local settings.


Details
-------

- Multiple ``.sidecar-locals`` paths are supported.

- Multiple major-modes are supported (so ``c-mode`` will detect both ``c-mode`` and ``prog-mode``).

- The execution order of locals is always least to most specific,
  so top-level locals are executed before those found further down the directory hierarchy.
  Derived modes such as ``prog-mode`` are executed before the major-mode that derived from them.

  This is done so it's possible to setup generic settings that can be overridden by more specific locations & modes.

- Nested sub-directories in ``.sidecar-locals`` are supported,
  you may mirror the directory structure of your project if you need subdirectories to have their own configuration.

  For example:

  .. code-block::

     /src/
     /src/my-project/
     /src/my-project/external/
     /src/.sidecar-locals/
     /src/.sidecar-locals/my-project(c-mode).el
     /src/.sidecar-locals/my-project/external(c-mode).el

- This package doesn't conflict with ``dir-locals`` (which run beforehand).


Security
--------

Since running code directly has security implications, here are some suggestions for how to use this package safely.

For public projects it's recommended to create ``.sidecar-locals`` above the projects version control root,
and only trust that directory.

This way any ``.sidecar-locals`` directories committed into the project will be detected and reported as untrusted
(without running any code).

For personal projects when you aren't concerned someone else creating a ``.sidecar-locals`` directory
you may add ``.sidecar-locals`` anywhere in the project,
recursively trusting the projects root using the ``*`` path suffix.


Installation
============

.. code-block:: elisp

   (use-package sidecar-locals
     :straight
     (sidecar-locals
       :type git
       :host gitlab
       :repo "ideasman42/emacs-sidecar-locals"))

   (sidecar-locals-mode)
