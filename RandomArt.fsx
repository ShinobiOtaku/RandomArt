type expr =
   | VariableX
   | VariableY
   | Constant
   | Sin of expr
   | Cos of expr
   | Tan of expr
   | Sinh of expr
   | Cosh of expr
   | Tanh of expr
   | Mod of expr * expr

let random = System.Random()
let next () = random.NextDouble()

let rec eval = function
   | VariableX -> fun (x,y) -> (x,x,x)
   | VariableY -> fun (x,y) -> (y,y,y)
   | Constant -> 
      let r,g,b = next(),next(),next()
      fun (x,y) -> (r,g,b)
   | Mod(e1,e2) ->
      let f1,f2 = eval e1, eval e2
      fun (x,y) ->
         let r1,g1,b1 = f1(x,y)
         let r2,g2,b2 = f2(x,y)
         r1 % r2, g1 % g2, b1 % b2
   | Sin(e) ->
      let f = eval e
      let phase = next() * System.Math.PI
      let freq = (next()*5.0)+1.0      
      fun (x,y) ->
         let r,g,b = f(x,y)
         sin(phase + r*freq),sin(phase+g*freq),sin (phase+b*freq)
   | Cos(e) ->
      let f = eval e
      let phase = next() * System.Math.PI
      let freq = (next()*5.0)+1.0      
      fun (x,y) ->
         let r,g,b = f(x,y)
         cos(phase + r*freq),cos(phase+g*freq),cos (phase+b*freq)
   | Tan(e) ->
      let f = eval e
      let phase = next() * System.Math.PI
      let freq = (next()*5.0)+1.0      
      fun (x,y) ->
         let r,g,b = f(x,y)
         tan(phase + r*freq),tan(phase+g*freq),tan (phase+b*freq)   
   | Sinh(e) ->
      let f = eval e
      let phase = next() * System.Math.PI
      let freq = (next()*5.0)+1.0      
      fun (x,y) ->
         let r,g,b = f(x,y)
         sinh(phase + r*freq),sinh(phase+g*freq),sinh (phase+b*freq)
   | Cosh(e) ->
      let f = eval e
      let phase = next() * System.Math.PI
      let freq = (next()*5.0)+1.0      
      fun (x,y) ->
         let r,g,b = f(x,y)
         cosh(phase + r*freq),cosh(phase+g*freq),cosh (phase+b*freq)
   | Tanh(e) ->
      let f = eval e
      let phase = next() * System.Math.PI
      let freq = (next()*5.0)+1.0      
      fun (x,y) ->
         let r,g,b = f(x,y)
         tanh(phase + r*freq),tanh(phase+g*freq),tanh (phase+b*freq)       

let rec gen k =
   if k <= 0 || next() < 0.01 then
      let terminals = [VariableX; VariableY;Constant]
      terminals.[random.Next(terminals.Length)]
   else
      let n () = random.Next(k)
      let f e = fun() -> e (gen (n()))
      let operators = [
         fun () -> Mod(gen (n()), gen(n()))
         f Sin
         f Cos
         f Tan
         f Sinh
         f Cosh
         f Tanh
      ]
      operators.[random.Next(operators.Length)]()

#if INTERACTIVE
#r "System.Drawing.dll"
#endif

let rgb (r,g,b) =
   let r = max 0 (min 255 (int (128.0 * (r + 1.0))))
   let g = max 0 (min 255 (int (128.0 * (g + 1.0))))
   let b = max 0 (min 255 (int (128.0 * (b + 1.0))))
   r,g,b
 
let width, height = 1280, 720

open System
open System.IO
open System.Drawing

let draw f n =
   let image = new Bitmap(width, height)
   [|for y in 0..n..height-n do
      for x in 0..n..width-n -> x,y|]
   |> Array.Parallel.map (fun (x,y) ->
      let u = -1.0 + (((float x+(float n/2.0))*2.0)/float width)
      let v = -1.0 + (((float y+(float n/2.0))*2.0)/float height)
      let r,g,b = f(u,v)
      let r,g,b = rgb(r,g,b)
      x,y,r,g,b
   )
   |> Array.iter (fun (x,y,r,g,b) ->         
      image.SetPixel(x,y,Color.FromArgb(r,g,b))
   )
   image

let show n =
   let e = gen 20
   let f = eval e
   let image = draw f 1
   let filename = sprintf "Random%04d.png" n
   let filePath = Path.Combine(__SOURCE_DIRECTORY__, filename)
   image.Save(filePath, Imaging.ImageFormat.Png)
        
show 9999
printf "done"