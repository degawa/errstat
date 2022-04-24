---
output_dir: ./api-doc
src_dir: ./src
media_dir: ./doc/media
exclude_dir: ./test
             ./example
project: Errstat
project_github: https://github.com/degawa/errstat
summary: An error status and message handling library for Modern Fortran
author: Tomohiro Degawa
license: by-sa
docmark: !
docmark_alt: *
predocmark: >
predocmark_alt: |
display: public
         protected
         private
sort: permission-alpha
search: true
source: false
extra_mods: iso_fortran_env: https://gcc.gnu.org/onlinedocs/gfortran/ISO_005fFORTRAN_005fENV.html
            iso_c_binding: https://gcc.gnu.org/onlinedocs/gfortran/ISO_005fC_005fBINDING.html
            ieee_arithmetic: https://gcc.gnu.org/onlinedocs/gfortran/IEEE-modules.html
graph: false
coloured_edges: true
print_creation_date: true
creation_date: %Y-%m-%d %H:%M %z
md_extensions: markdown.extensions.toc
               markdown.extensions.smarty
---

<!-- document's top page content --->
{!api-doc-index.md!}