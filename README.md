# Description

Uva honours project with the goal of proving confluence and termination of the DDRSs described in https://arxiv.org/abs/1406.3280 after certain changes to the rules. 

# Contents of directory

## Rules 

Contains directories 'folded' and 'unfolded'. The files in 'folded' serve as recipes for .trs files, mirroring the notation used in the aforementioned paper. They can be unfolded using unfold.py. Their unfolded versions are placed in 'unfolded'.

## unfold.py

Call using: 

$ python unfold.py [input_path] [output_path]

Input_path must be specified, output_path will default to input_path
but with .recipe replaced with .trs.

## Archive

Old files.

# Termination and confluence tools used.

Confluence

- CSI: http://colo6-c703.uibk.ac.at/hzankl/csi/interface/index.php

- ACP: http://www.nue.riec.tohoku.ac.jp/tools/acp/ 

Termination 

- Aprove: http://aprove.informatik.rwth-aachen.de/index.asp?subform=termination_proofs.html

- TTT2: http://cl-informatik.uibk.ac.at/software/ttt2/ 

