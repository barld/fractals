module HTree

open System
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input

let screenWidth = 1600
let screenHeigth = 1000

type Orientation = Horizontal | Vertical

type HTreeLine = {startPoint:Vector2; endPoint:Vector2;child1:Option<HTreeLine>;child2:Option<HTreeLine>; orientation:Orientation; length:float32}

let decreasingFactor = 1.f / (Math.Sqrt(2.) |> float32)

let buildTree (depth:int) =
    let rec buildLoop depth (orientation:Orientation) (point:Vector2) (length:float32) =
        if depth = 0 then
            None
        else if orientation = Vertical then
            let startPoint = point - new Vector2(0.f,length/2.f)
            let endPoint = point + new Vector2(0.f,length/2.f)
            Some {
                startPoint = startPoint
                endPoint = endPoint
                child1 = buildLoop (depth-1) Horizontal startPoint (length*decreasingFactor)
                child2 = buildLoop (depth-1) Horizontal endPoint (length*decreasingFactor)
                orientation = orientation
                length = length
            }
        else
            let startPoint = point - new Vector2(length/2.f,0.f)
            let endPoint = point + new Vector2(length/2.f,0.f)
            Some {
                startPoint = startPoint
                endPoint = endPoint
                child1 = buildLoop (depth-1) Vertical startPoint (length*decreasingFactor)
                child2 = buildLoop (depth-1) Vertical endPoint (length*decreasingFactor)
                orientation = orientation
                length = length
            }
    let startLength = (screenWidth |> float32) / 2.4f
    let center = new Vector2((screenWidth |> float32)/2.f,1000.f/2.f)
    let startPoint = center - new Vector2(startLength/2.f,0.f)
    let endPoint = center + new Vector2(startLength/2.f,0.f)
    {
        startPoint = startPoint
        endPoint = endPoint
        child1 = buildLoop (depth-1) Vertical startPoint (startLength*decreasingFactor)
        child2 = buildLoop (depth-1) Vertical endPoint (startLength*decreasingFactor)
        orientation = Horizontal
        length = startLength
    }
        
let tree = buildTree 12

type Simulation() as this =
    inherit Game()

    let graphics = new GraphicsDeviceManager(this)
    let mutable spritebatch = Unchecked.defaultof<SpriteBatch>
    let mutable emptyPixel = Unchecked.defaultof<Texture2D>

    let drawLine (spriteBatch:SpriteBatch) (startPoint:Vector2) (endPoint:Vector2) =
        let edge = endPoint - startPoint
        let angle = Math.Atan2(float edge.Y, float edge.X) |> float32

        spriteBatch.Draw(emptyPixel,new Rectangle(int startPoint.X,int startPoint.Y,(edge.Length() |> int),1),System.Nullable(),Color.White,angle,new Vector2(0.f,0.f),SpriteEffects.None,0.f )

    override this.Initialize() = 
        graphics.PreferredBackBufferHeight <- screenHeigth
        graphics.PreferredBackBufferWidth <- screenWidth
        
        graphics.ApplyChanges()
        //graphics.ToggleFullScreen()
        do base.Initialize()

    override this.LoadContent() =
        do spritebatch <- new SpriteBatch(this.GraphicsDevice)
        do emptyPixel <- new Texture2D(this.GraphicsDevice, 1, 1)
        emptyPixel.SetData([|Color.White|])

    override this.Update(gameTime) =
        let dt = gameTime.ElapsedGameTime.TotalSeconds |> float32

        ()

    override this.Draw(gameTime) =
        do this.GraphicsDevice.Clear Color.Black
        spritebatch.Begin(SpriteSortMode.Immediate, BlendState.AlphaBlend);

        let rec HTreeDrawwer (tree:HTreeLine) =
            drawLine spritebatch tree.startPoint tree.endPoint
            let drawOptionChild (optionChild:Option<HTreeLine>) : Unit =
                match optionChild with
                | Some(child) -> HTreeDrawwer child
                | None -> ()
            drawOptionChild tree.child1
            drawOptionChild tree.child2

        HTreeDrawwer tree

        spritebatch.End()
        ()