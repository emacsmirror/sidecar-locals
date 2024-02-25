
##########
Change Log
##########


Version 0.2
===========

- 2024-02-27
  - Add ``sidecar-locals-root`` macro to access the project path sidecar-locals references.


Version 0.1
===========

- 2022-12-09
  - Add support for expanding ``~`` to the users HOME directory for
    ``sidecar-locals-paths-allow`` & ``sidecar-locals-paths-deny``.

- 2022-12-08
  - Add support for non-file buffers such as ``dired``, so changing directories may run sidecar-locals,
    matching dir-locals behavior.

- 2022-09-16
  - Show all potential sidecar-locals files in ``sidecar-locals-report`` without checking their trust status.
  - Fix accessing files under the HOME directory.

- 2022-04-21
  - ``sidecar-locals-report`` now reports notes when files are found.
