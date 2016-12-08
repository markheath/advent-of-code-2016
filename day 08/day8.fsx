open System.Text.RegularExpressions

type Instruction = Rect of int*int | RotateRow of int*int | RotateCol of int*int

let (|Matches|) pattern =
  let rgx = Regex (pattern, RegexOptions.Compiled)
  fun input -> [for m in rgx.Matches input do for g in m.Groups -> g.Value]

let (|Int|_|) str = match System.Int32.TryParse str with true, value -> Some value | _ -> None

let parseInstruction = function 
    | Matches @"rect (\d+)x(\d+)" [_; Int width; Int height] -> Rect(width,height)
    | Matches @"rotate row y\=(\d+) by (\d+)" [_; Int row; Int steps] -> RotateRow(row,steps)
    | Matches @"rotate column x\=(\d+) by (\d+)" [_; Int row; Int steps] -> RotateCol(row,steps)
    | x -> failwith ("parseError " + x)
  
let instructions = System.IO.File.ReadAllLines (__SOURCE_DIRECTORY__ + "\\input.txt") |> Array.map parseInstruction

let rect (width,height) (state:int[,]) = 
    let out = Array2D.copy state
    for w in 0..width-1 do
        for h in 0..height-1 do
            out.[h,w] <- 1
    out

let rotateRow (row,steps) (state:int[,]) = 
    let out = Array2D.copy state
    let rowLength = Array2D.length2 state
    for w in 0..rowLength-1 do
        out.[row,(w+steps)%rowLength] <- state.[row,w]
    out
  
let rotateCol (col,steps) (state:int[,]) = 
    let out = Array2D.copy state
    let colLength = Array2D.length1 state
    for w in 0..colLength-1 do
        out.[(w+steps)%colLength,col] <- state.[w,col]
    out 

let applyInst (state:int[,]) = function
    | Rect(a,b) -> rect (a,b) state
    | RotateRow(a,b) -> rotateRow (a,b) state
    | RotateCol(a,b) -> rotateCol (a,b) state

[| Rect (3,2) ; RotateCol (1,1) ; RotateRow (0,4); RotateCol (1,1)  |]
    |> Array.fold applyInst (Array2D.create 3 7 0)
    |> Seq.cast<int>
    |> Seq.sum
    |> printfn "Test: %d"

instructions 
    |> Array.fold applyInst (Array2D.create 6 50 0) 
    |> Seq.cast<int> 
    |> Seq.sum 
    |> printfn "Part a: %d" // 116

let endState = instructions |> Array.fold applyInst (Array2D.create 6 50 0) // UPOJFLBCEZ

(* TO SAVE AS BITMAP
open System.Windows.Forms
open System.Drawing

let drawState (state:int[,]) =
    let width =Array2D.length2 state
    let height = Array2D.length1 state
    let bm = new Bitmap (width, height)
    for x in {0..width-1} do
        for y in {0..height-1} do
            bm.SetPixel(x,y, if state.[y,x] = 1 then Color.Black else Color.White)
    bm

let image  = drawState endState 
//image.Save(imagePath)
*)

#if INTERACTIVE
#r "PresentationCore.dll"
#r "PresentationFramework.dll"
#r "System.Xaml.dll"
#r "UIAutomationTypes.dll"
#r "WindowsBase.dll"
#endif

open System.Windows
open System.Windows.Media
open System.Windows.Media.Imaging
open System.Windows.Shapes
open System.Windows.Controls

let drawState (state:int[,]) scale =
   
    let width = Array2D.length2 state
    let height = Array2D.length1 state
    let canvas = new Canvas(Background=Brushes.DarkGreen,
                            RenderTransform=ScaleTransform(ScaleX = scale, ScaleY = scale),
                            Width = (float width),
                            Height = (float height),
                            HorizontalAlignment = HorizontalAlignment.Left,
                            VerticalAlignment = VerticalAlignment.Top
                            )
    for x in {0..width-1} do
        for y in {0..height-1} do
            if state.[y,x] = 1 then 
                let p = Rectangle(Width=1.0,Height=1.0,Fill=Brushes.Lime)
                canvas.Children.Add(p) |> ignore
                Canvas.SetLeft(p, float x)
                Canvas.SetTop(p, float y)
    printfn "%f %f" canvas.Width canvas.Height    
    canvas

let scaleFactor = 10.0
let canvas = drawState endState scaleFactor
let window = Window(Title ="Advent of Code", Content=canvas, 
                    Width = 20.0 +  canvas.Width * scaleFactor, Height = 40.0 + canvas.Height * scaleFactor)
window.ShowDialog()