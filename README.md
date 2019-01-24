| Week |                        Dates, starting on Monday                        |                                                                       Focus                                                                      |
|:----:|:-----------------------------------------------------------------------:|:------------------------------------------------------------------------------------------------------------------------------------------------:|
|   1  | 29 Oct - 04 Nov                                                         | Consult more related work. Background reading. Unikernels and Library OS FFIs Experiment with Mirage Experiment with Idris Experiment with Ocaml |
|   2  | 05 Nov - 11 Nov                                                         |                                                                                                                                                  |
|   3  | 12 Nov - 18 Nov                                                         |                                                                                                                                                  |
|   4  | 19 Nov - 25 Nov                                                         |                                                                                                                                                  |
|   5  | 26 Nov - 02 Dec                                                         |                                   Configure development environtment Requirements gathering Build Specification                                  |
|   6  | 03 Dec - 09 Dec                                                         |                                                                                                                                                  |
|   7  | 10 Dec - 16 Dec                                                         |                                                              Experiment with Mirage                                                              |
|   8  | 17 Dec - 23 Dec                                                         |                               Start implementing small bits  of the ffi, esentially work towards the 1st objective                               |
|   9  | 24 Dec - 30 Dec                                                         |                                                       and work towards the basic objective                                                       |
|  10  | 31 Dec - 06 Jan                                                         |                                                            Fix Idris ergonomic issues                                                            |
|  11  | 07 Jan - 13 Jan !!! Friday 11, 09:00 Progress Presentation              |                                      Preliminary progress presentation reparation -plan for the 2nd semester                                     |
|  12  | 14 Jan - 20 Jan                                                         |                                             Implement basic deliverable and start writing the report                                             |
|  13  | 21 Jan - 27 Jan                                                         |                                                                                                                                                  |
|  14  | 28 Jan - 03 Feb                                                         |                                             Implement the intermediate deliverable and report Testing                                            |
|  15  | 04 Feb - 10 Feb                                                         |                                                                                                                                                  |
|  16  | 11 Feb - 17 Feb                                                         |                                             Attempting the advanced deliverable Report Testing Mirage                                            |
|  17  | 18 Feb - 24 Feb                                                         |                                                                                                                                                  |
|  18  | 25 Feb - 03 Mar                                                         |                                                                                                                                                  |
|  19  | 04 Mar - 10 Mar                                                         |                                     Validation Verification Proofread Refactor Finish Writing up Prepare Demo                                    |
|  20  | 11 Mar - 17 Mar                                                         |                                                                                                                                                  |
|  21  | 18 Mar - 24 Mar !!! Need to be ready by now                             |                                                                                                                                                  |
|  22  | 25 Mar - 27 Mar !!! Submission Monday 25, 12:00 Demo is on Wednesday 27 |                                                                      Hooray!                                                                     |

# Idris backend for Malfunction

Compiles Idris to [Malfunction](https://github.com/stedolan/malfunction).

It seems to go pretty fast:
    
    $ idris pythag.idr -o pythag-idris
    $ idris pythag.idr --codegen malfunction -o pythag-malfunction
    
    $ time ./pythag-idris  > /dev/null
   
    real    0m4.548s
    user    0m4.528s
    sys     0m0.016s
    
    $ time ./pythag-malfunction  > /dev/null
    
    real    0m0.654s
    user    0m0.652s
    sys     0m0.000s

Tested on:
* Ubuntu `16.04.4 LTS 64-bit`
* Intel® Core™ i5-2500 CPU @ 3.30GHz × 4 
* 8 GB RAM
* Idris `1.2.0`
* Malfunction `v0.2.1`
* OCaml `4.05.0+flambda`