#r "System.Drawing.dll"

open System.Drawing

// Create bitmap

let rand n = 
   let r = System.Random()
   r.Next(1, 20) |> float

let width, height = 1280, 720
let image = new Bitmap(width, height)
for y = 0 to height - 1 do
   for x = 0 to width - 1 do
      let u = -1.0 + (float x * 2.0) / float width
      let v = -1.0 + (float y * 2.0) / float height
      let r = u * (u |> rand)
      let g = v * (v |> rand)
      let b = u * (v |> rand)
      let norm n = int (128.0 * (n + 1.0)) |> min 255 |> max 0
      image.SetPixel(x,y,Color.FromArgb(norm r,norm g,norm b))

// Save bitmap
open System.IO
let filePath = Path.Combine(__SOURCE_DIRECTORY__, "z.png")
image.Save(filePath, Imaging.ImageFormat.Png)
