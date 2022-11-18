Experimental proc-macro generating partially inhabited enums from a template
enum and valid morphisms between those enums. The goal is to define an enum
with all possible variants once and generate the partial enums to constrain
different APIs to different variant subsets, without having to redefine new
enums for each API. Generated morphisms can then be used to convert between
those different enums and easily compose APIs.
