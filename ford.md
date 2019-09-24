src_dir: ./doc_files
output_dir: ./docs/document
author: Wang Chao & Dong Shaojun
project: TNSG
fpp_extensions: fpp
predocmark: >
media_dir: ./media
docmark_alt: #
predocmark_alt: <
display: public
source: false
graph: true
search: true
macro: TEST
       LOGIC=.true.
extra_mods: json_module: http://jacobwilliams.github.io/json-fortran/
            futility: http://cmacmackin.github.io
license: by-nc
extra_filetypes: sh #

This is the documentation page of Tensor Network State Git(TNSG), a highly efficient tensor network package written in Fortran 2003. For more information about the package, please visit our [homepage](https://crimestop.github.io/tenpack/) or the [github page](https://github.com/crimestop/tenpack).

The following are the libraries TNSG provides and the modules they contain

- ltnsp (<span style="color:#c91b00">rely on</span>  none)
    - tensor_type: a class to operate on tensors
    - tools: some useful tensor functions
- ltnsp_ext (<span style="color:#c91b00">rely on</span> ltnsp lrandom lkernel)
    - tnsp_ext: some extra tensor functions
- ltenwork (<span style="color:#c91b00">rely on</span> ltnsp ldic lkernel)
    - tensor_network: a class to operate on tensor networks
    - tn_tensor_type: a class to deal with tensor contraction
    - tensor_network_nesting: a class to operate on nested tensor networks
- ltimer (<span style="color:#c91b00">rely on</span> lkernel)
    - timer: a class to benchmark the program
- lparalist (<span style="color:#c91b00">rely on</span> lkernel ldic)
    - parameter_list: a class to manage the parameters
- lcumu (<span style="color:#c91b00">rely on</span> ltnsp ltnsp_ext lkernel)
    - mod_cumu: a class to cumulate a value
- lrandom (<span style="color:#c91b00">rely on</span> lkernel)
    - mod_rand: a class to generate random numbers
- ldic (<span style="color:#c91b00">rely on</span> lkernel ltnsp)
    - mod_dictionary: a class to define python-type dictionary
- lkernel (<span style="color:#c91b00">rely on</span> ltnsp)
    - string and all other modules: some kernel modules

All the libraries are documented here except **ltnsp**, which deserves its own documentation page.