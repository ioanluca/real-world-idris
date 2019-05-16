import OCaml.IO

front_legss : List (Int, Int)
front_legss = [
        (256, 246), (249, 287), (251, 343), (264, 346), (266, 306), (276, 276),
        (282, 306), (278, 343), (292, 343), (298, 306), (298, 277), (299, 254), 
        (255, 246)
    ]

back_legss : List (Int, Int)
back_legss = [
         (432, 243), (441, 289), (430, 334), (445, 334), (462, 275), (469, 328),
         (476, 328), (478, 259), (454, 214), (461, 164), (407, 206)
    ]

set_color : Int -> OCaml_IO ()
set_color = ocamlCall "Graphics.set_color" (Int -> OCaml_IO ())

fe : Int -> Int -> Int -> Int -> Int -> OCaml_IO ()
fe y_max x y w h = let y = y_max - y in
    ocamlCall "Graphics.fill_ellipse" (Int -> Int -> Int -> Int -> OCaml_IO ())
     (x + (w `div` 2)) (y - (h `div` 2)) (w `div` 2) (h `div` 2)

fp : Int -> Ptr -> OCaml_IO ()
fp y_max ar = do 
    f <- ocamlCall "Idrisobj.f" (Int -> OCaml_IO Ptr) y_max
    arr <- ocamlCall "Array.map" 
        (Ptr -> Ptr -> OCaml_IO Ptr) f ar
    ocamlCall "Graphics.fill_poly" (Ptr -> OCaml_IO ()) arr


loop : () -> OCaml_IO ()
loop _ = do
    ocamlCall "Graphics.wait_next_event" (List Int -> OCaml_IO Ptr) 
        [0,1,2,3,4]
    loop ()

main : OCaml_IO  ()
main = do
    ocamlCall "Graphics.open_graph" (String -> OCaml_IO ()) ""
    ocamlCall "Graphics.auto_synchronize" (Bool -> OCaml_IO ()) False
    ocamlCall "Graphics.clear_graph" (() -> OCaml_IO ()) ()
    ocamlCall "Graphics.synchronize" (() -> OCaml_IO ()) ()

    y_max <- ocamlCall "Graphics.size_y" (() -> OCaml_IO Int) ()

    orange <- ocamlCall "Graphics.rgb" (Int -> Int -> Int -> OCaml_IO Int)
        198 141 62
    white <- ocamlCall "Graphics.white" (OCaml_IO Int)
    black <- ocamlCall "Graphics.black" (OCaml_IO Int)

    back_legs <- ocamlCall "Idrisobj.back_legs" (OCaml_IO Ptr)
    front_legs <- ocamlCall "Idrisobj.front_legs" (OCaml_IO Ptr)

    set_color orange
    fe y_max 185 90 250 147
    fe y_max 269 54 68 98
    fe y_max 143 138 127 94

    set_color white
    fe y_max 89 (-79) 195 227

    set_color orange
    fe y_max 134 93 62 122
    fe y_max 97 101 86 47 
    fe y_max 354 63 68 118 
    fe y_max 367 101 98 109
    fe y_max 247 176 68 94 

    fp y_max back_legs
    fp y_max front_legs

    ocamlCall "Graphics.moveto" (Int -> Int -> OCaml_IO ()) 200 40
    set_color black

    ocamlCall "Graphics.draw_string" (String -> OCaml_IO ()) 
        "Bactrian the Double-Humped OCaml"

    ocamlCall "Graphics.synchronize" (() -> OCaml_IO ()) ()
    loop ()



