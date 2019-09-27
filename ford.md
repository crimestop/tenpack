src_dir: ./doc_files
favicon: ./docs/pic/icon.png
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
graph: false
search: false
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
    - <a href='./module/tnsp_ext.html'>tnsp_ext</a>: some extra tensor functions
- ltenwork (<span style="color:#c91b00">rely on</span> ltnsp ldic lkernel)
    - <a href='./module/tensor_network.html'>tensor_network</a>: a class to operate on tensor networks
    - <a href='./module/tn_tensor_type.html'>tn_tensor_type</a>: a class to deal with tensor contraction
    - <a href='./module/tensor_network_nesting.html'>tensor_network_nesting</a>: a class to operate on nested tensor networks
- ltimer (<span style="color:#c91b00">rely on</span> lkernel)
    - <a href='./module/timer.html'>timer</a>: a class to benchmark the program
- lparalist (<span style="color:#c91b00">rely on</span> lkernel ldic)
    - <a href='./module/parameter_list.html'>parameter_list</a>: a class to manage the parameters
- lcumu (<span style="color:#c91b00">rely on</span> ltnsp ltnsp_ext lkernel)
    - <a href='./module/mod_cumu.html'>mod_cumu</a>: a class to cumulate a value
- lrandom (<span style="color:#c91b00">rely on</span> lkernel)
    - <a href='./module/mod_rand.html'>mod_rand</a>: a class to generate random numbers
- ldic (<span style="color:#c91b00">rely on</span> lkernel ltnsp)
    - <a href='./module/mod_dictionary.html'>mod_dictionary</a>: a class to define python-type dictionary
- lkernel (<span style="color:#c91b00">rely on</span> none)
    - <a href='./module/string.html'>string</a> and all other modules: some kernel modules

All the libraries are documented here except **ltnsp**, which deserves its own documentation page.

You may browse:

- <a href='./lists/files.html'>source files</a>
- <a href='./lists/modules.html'>modules</a>
- <a href='./lists/procedures.html'>procedures</a>
- <a href='./lists/types.html'>derived types</a>
