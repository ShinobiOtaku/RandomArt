type expr =
   | VariableX
   | VariableY
   | Const
   | Red
   | Green
   | Blue
   | Sum of expr * expr
   | Product of expr * expr
   | Mod of expr * expr
   | Well of expr
   | Pow of expr
   | Tent of expr
   | Sin of expr
   | Cos of expr
   | Tan of expr
   | Rand of expr
   | Level of expr * expr * expr
   | Mix of expr * expr * expr

let random = System.Random()
let next () = random.NextDouble()

let average (c1,c2,w) =
    let r1,g1,b1 = c1
    let r2,g2,b2 = c2
    let r = w * r1 + (1.0 - w) * r2
    let g = w * g1 + (1.0 - w) * g2
    let b = w * b1 + (1.0 - w) * b2
    r,g,b

let well x = 1.0 - 2.0 / (1.0 + x * x) ** 8.0

let tent x = 1.0 - 2.0 * abs x

let rec eval = function
   | VariableX -> fun (x,y) -> (x,x,x)
   | VariableY -> fun (x,y) -> (y,y,y)
   | Const -> 
      let r,g,b = next(), next(), next()
      fun (x,y) -> (r,g,b)
   | Red -> 
      let red = next();
      fun (x,y) -> (red,0.0,0.0)
   | Green -> 
      let green = next()
      fun (x,y) -> (0.0,green,0.0)
   | Blue -> 
      let blue = next()
      fun (x,y) -> (0.0, 0.0, blue)
   | Sum(e1,e2) ->
      let f1,f2 = eval e1, eval e2
      fun (x,y) ->
         average(f1(x,y),f2(x,y),0.5)
   | Product(e1,e2) ->
      let f1,f2 = eval e1, eval e2
      fun (x,y) ->
         let r1,g1,b1 = f1(x,y)
         let r2,g2,b2 = f2(x,y)
         r1*r2, g1*g2, b1*b2
   | Mod(e1,e2) ->
      let f1,f2 = eval e1, eval e2
      fun (x,y) ->
         let r1,g1,b1 = f1(x,y)
         let r2,g2,b2 = f2(x,y)
         r1 % r2, g1 % g2, b1 % b2
   | Well(e) ->
      let f = eval e
      fun (x,y) ->         
         let r,g,b = f(x,y)
         well r, well g, well b
   | Pow(e) ->
      let f = eval e
      let phase = next() * System.Math.PI
      let freq = (next()*5.0)+1.0 |> float      
      fun (x,y) ->
         let r,g,b = f(x,y)
         System.Math.Pow(r, phase + freq),System.Math.Pow(g, phase + freq),System.Math.Pow(b, phase + freq)
   | Tent(e) ->
      let f = eval e
      fun (x,y) ->         
         let r,g,b = f(x,y)
         tent r, tent g, tent b
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
   | Rand(e) -> fun (x,y) ->
      (next() * System.Math.PI |> float), (next() * System.Math.PI |> float), (next() * System.Math.PI |> float)
   | Level(e1,e2,e3) ->
      let f1,f2,f3 = eval e1, eval e2, eval e3
      let threshold = (next()*2.0) - 1.0
      fun (x,y) ->
         let r1,g1,b1 = f1(x,y)
         let r2,g2,b2 = f2(x,y)
         let r3,g3,b3 = f3(x,y)
         let r = if r1 < threshold then r2 else r3
         let g = if g1 < threshold then g2 else g3
         let b = if b1 < threshold then b2 else b3
         r,g,b
   | Mix(e1,e2,e3) ->
      let f1,f2,f3 = eval e1, eval e2, eval e3
      let threshold = (next()*2.0) - 1.0
      fun (x,y) ->
         let n, _, _ = f1(x,y)
         let w = 0.5 * (n + 1.0)
         let c1 = f2(x,y)
         let c2 = f3(x,y)
         average(c1,c2,w)

let rec gen k =
   if k <= 0 || next() < 0.01 then
      let terminals = [Const; VariableX; VariableY]
      terminals.[random.Next(terminals.Length)]
   else
      let n () = random.Next(k)
      let operators = [
         fun () -> Sum(gen (n()), gen(n()))
         fun () -> Product(gen (n()), gen(n()))
         fun () -> Mod(gen (n()), gen(n()))
         fun () -> Well(gen (n()))
         fun () -> Tent(gen (n()))
         fun () -> Sin(gen (n()))
         //fun () -> Sin(gen (n()))
         fun () -> Cos(gen (n()))
         //fun () -> Cos(gen (n()))
         fun () -> Tan(gen (n()))
         //fun () -> Tan(gen (n()))
         //fun () -> Rand(gen (n()))
         //fun () -> Pow(gen (n()))
         fun () -> Level(gen (n()), gen (n()), gen(n()))
         fun () -> Mix(gen (n()), gen (n()), gen(n()))
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
 
let width, height = 512, 384

open System
open System.Drawing

let draw f n =
   let image = new Bitmap(width, height)
   use graphics = Graphics.FromImage(image)  
   [|for y in 0..n..height-n do
      for x in 0..n..width-n -> x,y
   |]
   |> Array.map (fun (x,y) ->
         let x1 = -1.0 + (((float x+(float n/2.0))*2.0)/float width)
         let y1 = -1.0 + (((float y+(float n/2.0))*2.0)/float height)
         let r,g,b = f(x1,y1)
         let r,g,b = rgb(r,g,b)
         x,y,r,g,b
   )
   |> Array.iter (fun (x,y,r,g,b) ->        
      use pen = new SolidBrush(Color.FromArgb(r,g,b))
      graphics.FillRectangle(pen, x, y, n, n)
   )
   image




let save n =
   let e = gen 50
   let f = eval e
   let image = draw f 1
   //let path = System.IO.Path.Combine(__SOURCE_DIRECTORY__, let filename = sprintf "Random%04d.png" n)
   let path = sprintf @"%s\temp7\Random%04d.png" __SOURCE_DIRECTORY__ n
   printfn "%s" path
   image.Save(path, Imaging.ImageFormat.Png)
        
for i = 1 to 20 do save i