import Html exposing (div, input, Attribute)
import Browser
import Html.Events exposing (on, keyCode, onInput)
import Html.Attributes exposing (value)
import Json.Decode as Json
import Dict exposing (Dict)
import Round

--Main Menu
mainMenu model = [(Joshin10.LandingPage1.myShapes model.menuModel) 
                |> group
                |> GraphicSVG.map LandingMsg,
                rect 100 20 
                  |> filled black 
                  |> makeTransparent 0
                  |> notifyTap Docs,
               rect 100 20 
                  |> filled black 
                  |> makeTransparent 0
                  |> move (0, -25)
                  |> notifyTap ToApp] |> group |> scale model.mainShow
 
docs model =  [(Joshin10.Docs0.myShapes model.docModel) 
                |> group
                |> GraphicSVG.map DocsMsg,
                
                rect 50 10 
                  |> filled green 
                  |> makeTransparent 0
                  |> move (-55, -53)
                  |> notifyTap ToApp] |> group |> scale model.docShow
 
textCol model = if model.theme then white else black              
drawTextB displayText s pos colour b =        
  text displayText
        |> sansserif
        |> (if b then bold else sansserif)
        |> size s
        |> filled colour
        |> move pos
  
-- Help Menus
accelhelpMenu model = 
  let accelMenu = terny (model.theme) (Johnsh20.Application.myShapes (Johnsh20.Application.init))
                                      (Johnsh20.Application1.myShapes (Johnsh20.Application1.init))                                    
  in
    group [ accelMenu |> group,
            rect 20 10 |> filled white |> makeTransparent 0 |> move (-75, 55) 
                       |> notifyTap ToggleAccelHelp,
             rect 20 15 |> filled (if model.theme then black else white)
                        |> move (-75, 40)] 
                        |> scale (terny (model.accelHelp) 1 0)

velhelpMenu model = 
  let velMenu = terny (model.theme) (Johnsh20.Application0.myShapes (Johnsh20.Application0.init))
                                      (Johnsh20.Application00.myShapes (Johnsh20.Application00.init))                                    
  in
    group [ velMenu |> group,
            rect 20 10 |> filled white |> makeTransparent 0  |> move (-75, 55) 
                       |> notifyTap ToggleVelHelp,
                       rect 20 15 |> filled (if model.theme then black else white) 
                       |> move (-75, 40)] 
                       |> scale (terny (model.velHelp) 1 0)
 
myShapes model =
  [ 
    rect 190 126 |> filled (if model.theme then black else white), 
    --Draws gridlines that move alongside the drag
    gridlines model |> move (model.dx, model.dy),
    -- Axis thats moves with dragging motion 
    axis model |> move (model.dx, model.dy),

      -- The actual function(s) that are drawn
      -- They move with dragging and any vertical/horizontal shifts applied
      draw model |> move (model.dx + model.fdx, model.dy + model.fdy) 
        |> notifyEnterAt ToolTip
        |> notifyLeaveAt ToolTip,
      
      menu model |> move (80, 50) |> scaleX model.menuShow,
      inputToggle model,
      menuToggle model,
      docsToggle model,
      textbox model |> scale (model.inputShow),
      
      [ circle 4 |> filled grey |> move (90, -45)  |> notifyTap ToggleAccelHelp,
          text "❓"
            |> size 4
            |> filled black
            |> move (88, -46)
            |> notifyTap ToggleAccelHelp
        ]
          |> group
          |> scale (terny (model.velHelp) 0 1)
          |> scaleX model.menuShow,      
      
      accelhelpMenu model,

     [ circle 4 |> filled grey |> move (90, -27)  |> notifyTap ToggleVelHelp,
          text "❓"
            |> size 4
            |> filled black
            |> move (88, -28)
            |> notifyTap ToggleVelHelp
        ]
          |> group  
          |> scale (terny (model.accelHelp) 0 1)
          |> scaleX model.menuShow,  
          
      velhelpMenu model,
       
      themeToggle model,
      
      -- Menu
      docs model,
      mainMenu model
  ]


-- Calculates the degree of the polynomial, splits by comma to get number of coefficents
degree_ model = toFloat (List.length (String.split "," model.f))

-- calculate factorials for derivatives
fact x y = 
  if x == 0 then y
  else fact (x-1) (x*y)


deriv n x model = 
                -- List of coefficents (characters)
            let coeffs = List.map (\c -> (Maybe.withDefault 1 (String.toFloat c))  ) (String.split "," model.f)
                -- Apply stretch factor
                stretch s = (if (degree_ model - n > 2)
                                then  (0.1^(degree_ model - 2 - n)) 
                                else 1)*(1/ (model.fdz))           
                -- Caluclates power, if power ends up being <= 0, it should be differentiated away and shouldnt be considered
                power i m = toFloat (i - m)
                --Coeffs
                dxcoeffs = List.drop 1 (List.indexedMap 
                               (\i e -> (e * (toFloat i))) 
                               coeffs)
                dx2coeffs = List.drop 1 (List.indexedMap 
                               (\i e -> (e * (toFloat i))) 
                               dxcoeffs)
                -- Constants
                constant1 = Maybe.withDefault 0 (List.head coeffs)
                constant2 = Maybe.withDefault 0 (List.head dxcoeffs)
                constant3 = Maybe.withDefault 0 (List.head dx2coeffs)
            
            in
               (List.sum
                  ( List.indexedMap
                    ( \i e ->
                      -- Evaluate x at the nth derivate
                      -- * (stretch n)
                            e * (stretch n) * x^(toFloat i)
                      )
                      -- Split string at commas to get array of coefficents
                      (if n == 0 then coeffs
                                 else if n == 1 
                                      then dxcoeffs
                                      else dx2coeffs)
                    ) 
                  ) +  (if n == 0 then (9)*constant1 
                        else if n == 1 then (9)*constant2 else (9)*constant3)

pretty model = 
    let l = List.indexedMap
              ( \i e ->
              -- Parse coefficents
                 times (toLatex (String.toList e) model) 
                       -- Ignore x^0
                        (if i == 0 then (ExprVar ' ')
                                    --Parses to x^i
                                    else if (i == 1)
                                    -- Don't want exponent of one to show
                                         then xExpr
                                         else (SuperSub xExpr [(ConstInt i )] []))
                       model)
              --Replace all commas and convert to char list
              (String.split "," model.f)            
  in case l of 
     -- If list is empty then default to graphing constant 0
     [] -> addExpr [] (ConstInt 0) model 
     -- Add up all terms together into one expression
     x::xs -> addExpr xs x model
            
addExpr l expr model = 
  case l of 
    -- If we have no terms left to add, return it
    [] -> expr
    -- Add next term and continue
    x::xs -> addExpr xs (plus x expr model) model

-- Returns colour scheme depending on if dark/light mode is enabled or not
theme model = if model.theme then white else black

-- Returns colour scheme for the drawn function depending on which theme is enabled and for each derivative

-- Dark   Light
funcTheme1 model = terny model.theme (rgb 0 158 115) (rgb 0 158 115)
funcTheme2 model = terny model.theme (rgb 204 121 167) (rgb 204 121 167)
funcTheme3 model = terny model.theme (rgb 0 114 178) (rgb 0 114 178)

--Returns keycode for press
onKeyDown tagger =
  on "keydown" (Json.map tagger keyCode)

textbox model =
  [ html
      150
      20
      ( div
          []
          -- Textbooks that sends Msg once enter is pressed
          [input [onKeyDown KeyDown, onInput Input, value model.input] [] ]
      ) 
      |> scale 0.5
      |> move (-50, 60)] 
    |> group

axis model =
  -- Size is determined by width (inclusive on both sides) which increases as user drags.
  let size = 800 
  in
    group
      [ rect size 0.2 |> filled (theme model),
        rect 0.2 size |> filled (theme model)]

gridlines model = 
    let size_ = 800
       -- Number of gridlines to display
        grids = 200 
       -- Get position to evenly space out the lines
        space_ y = 10*(toFloat y)
       -- Interval to draw gridlines on (this will get translated once user starts dragging)
        interval = (List.range -grids grids)
        -- Get theme colour
        colour_ = theme model
    in
      group [
              -- Horizontal gridlines
                List.map
                      (\x ->  
                          rect size_ 0.05 |> filled colour_ |> move (0, space_ x)
                       )
                      interval |> group,
              -- Vertical gridlines
                List.map
                      (\x ->  
                          rect 0.05 size_ |> filled colour_ |> move (space_ x,0)
                      )
                      interval |> group
              ]



draw model =
  let 
    -- We wanna grow the interval we graph based on where user has dragged to
    interval_radius = round (model.dx + model.fdx)
    -- Determine new interval based on pad
    interval = List.map (\x -> (toFloat x)/5)
               (List.range 
               (-500 - interval_radius ) 
               (500 - interval_radius))
    -- Scale point based on degree of polynomial so it fits on the screen
    new_ x = 5*x
    -- nth derivative
    f_prime n x mdl = deriv n (new_ x) mdl
  in
    (List.map (\(n, transp, colour)-> 
                          openPolygon
                            ( List.map
                            
                            -- Get coordinate at the nth derivative
                                (\x -> (new_ x, f_prime n x model))
                             -- Over the given interval 
                                interval
                            )
                            |> outlined (solid 1) colour
                            |> makeTransparent transp
                            
                     ) [(0, 1, funcTheme1 model), 
                        -- First derivative
                        (1, model.dxShow, funcTheme2 model), 
                        -- Second derivative
                        (2, model.dx2Show, funcTheme3 model)]) 
                     |> group

-- Hides/Shows menu
menuToggle model =
  [ circle 4 |> filled (if model.menuShow == 1 then lightBlue else grey) |> move (-80, 60) |> notifyTap ToggleMenu,
    text "👁"
      |> size 5
      |> filled (black)
      |> move (-82.5, 58)
      |> notifyTap ToggleMenu
  ]
    |> group

-- Toggles help menu
inputToggle model =
  [ circle 4 |> filled grey |> move (-70, 60),
    text "ƒ"
      |> size 5
      |> filled black
      |> move (-71.5, 59)
  ]
    |> group
    |> notifyTap TextShow

-- Toggles theme 
themeToggle model =
  group
    [ circle 4
        |> filled (theme model)
        |> move (-90, 60)
        |> notifyTap Theme,
      text "☼"
        |> size 5
        |> filled (if model.theme then black else white)
        |> move (-92.5, 58)
        |> notifyTap Theme
    ]
    
-- toggles documentation page
docsToggle model =
  group
    [ circle 4
        |> filled lightGray
        |> move (-60, 60)
        |> notifyTap Docs,
      text "?"
        |> size 5
        |> filled black
        |> move (-61, 58.5)
        |> notifyTap Docs
    ]


-- Replicates the ternary operator


terny bool valueIfTrue valueIfFalse = if bool then valueIfTrue else valueIfFalse

-- Takes a string, converts it to latex by abusing ExprVar

emptyLtx = ExprVar ' '

toLatex chrArr model = 
      case chrArr of
        [ ] -> ExprVar ' '
        [x] -> ExprVar x
        x::xs -> times (ExprVar x) (toLatex xs model) model
        
-- Helper functions for latex/text

drawText displayText pos colour =        
  text displayText
        |> sansserif
        |> centered
        |> size 5
        |> filled colour
        |> move pos
        
drawLatex expr pos model 
                     = [expr |> prettyExpr model |> parens Round |> showBox] 
                     |> group    
                     |> move pos
                     |> scale 0.5
                             
drawKey key pos pressed pCol model = 
  group [ square 5 |> outlined (solid 0.5) (if pressed then pCol else (theme model)) |> move pos
        , text key |> size 3.5 |> filled (if pressed then pCol else (theme model)) |> move (-1.5,-1) |> move pos ]

menu model =
  group
    [ rect 70 126 |> filled (terny model.theme black  white) |> move (0, -50),
      -- Displays equation currently being displayed
      drawLatex (pretty model) (-30 - 5*(toFloat (String.length model.f)), 0) model,

      -- Button to display first derivative  
      rect 30 10 |> filled (terny (model.dxShow == 1) (rgb 26 133 255) (rgb 212 17 89))
                  |> move (-10, -78) |> notifyTap DxShow,      
      drawText "Velocity" (-10, -80) (theme model)
               |> notifyTap DxShow,
               
       -- Button to display first derivative  
      rect 30 10 |> filled (terny (model.dx2Show == 1) (rgb 26 133 255) (rgb 212 17 89))
                 |> move (-10, -96) |> notifyTap Dx2Show,      
      drawText "Accel." (-10, -98) (theme model)
               |> notifyTap Dx2Show,
      
      -- Displays current transformations applied      
      rect 30 10 |> filled (rgb 120 94 240) |> move (-10, -10),
      drawText "[-]         Shifts         [+]" (-10, -12) (theme model),
      
      drawKey "W" (11, -21) (model.wDown) (rgb 120 94 240) model,
      drawKey "S" (-31, -21) (model.sDown) (rgb 120 94 240) model,
      drawKey "D" (11, -31) (model.dDown) (rgb 120 94 240) model,
      drawKey "A" (-31, -31) (model.aDown) (rgb 120 94 240) model,
      
      --Display vertical shift
      drawLatex 
        ( times ( toLatex (String.toList "y: ") model ) 
         (toLatex (String.toList ( Round.round 2 (model.fdy / 10))) model) model)
         (-50, -45) model,
         
      -- Display Horizontal shift   
      drawLatex 
         ( times ( toLatex (String.toList "x: ") model ) 
         (toLatex (String.toList ( Round.round 2 (model.fdx / 10))) model) model)
         (-50, -65) model,
          
      rect 30 10 |> filled (rgb 250 176 0) |> move (-10, -48),
      drawText "[-]       Stretch        [+]" (-10, -50) (theme model),
      
      --Display stretch factor
      drawLatex
         ( times ( toLatex (String.toList "s: ") model ) 
         ( toLatex ( String.toList ( Round.round 2 model.fdz)) model) 
         
         model)
         (-50, -125) model,
      drawKey "E" (11, -60) (model.eDown) (rgb 250 176 0) model,
      drawKey "Q" (-31, -60) (model.qDown) (rgb 250 176 0) model,
         
      let 
          pos_ = model.tooltipPos
          x = ((Tuple.first pos_) - model.dx)/10
          y = ((Tuple.second pos_) - model.dy)/10
      in
      -- Coordinates
        drawText 
            ("("  ++ (Round.round 2 x)
                  ++ "," ++ (Round.round 2 y) ++ 
             ")")
            (add_ (-60, -45) model.tooltipPos) (theme model)
           |> makeTransparent (terny (model.tooltipShow) 1 0),
           
      drawKey "←" (-25,-108) (model.leftDown) (rgb 0 114 178) model,
      drawKey "↑" (-15,-108) (model.upDown) (rgb 0 114 178) model,
      drawKey "→" (-5,-108) (model.rightDown) (rgb 0 114 178) model,
      drawKey "↓" (5,-108) (model.downDown) (rgb 0 114 178) model
          
    ]
    
    
type Msg = Tick Float GetKeyState
           | Theme 
           | Nothing
           | KeyDown Int
           | Input String
           | DxShow
           | Dx2Show
           | ToggleMenu
           | ToggleAccelHelp
           | TextShow
           | ToggleVelHelp
           | NewPos (Float,Float)
           | LetGo
           | ToolTip (Float,Float)
           | Docs
           | ToApp
           | LandingMsg Joshin10.LandingPage1.Msg
           | DocsMsg Joshin10.Docs0.Msg

type State 
  = Waiting
  | Dragging (Float,Float)

type alias Model = { time : Float,
                     dx : Float, -- Tracks change how much is dragged in x direction
                     dy : Float, -- Tracks change in y direction
                     fdy : Float, -- Tracks the vertical shift of the function 
                     fdx : Float, -- Tracks the horizontal shift of the function
                     fdz : Float, -- Tracks stretch if t
                     theme : Bool, -- State of light/dark mode
                     f : String, -- Function entered in textbox
                     input : String, -- What currently is in textbox, gets stored to model.f once enter is pressed
                     inputShow : Float, -- Visibility of textbox
                     dxShow : Float, -- Visibility of first derivative
                     dx2Show : Float, --Visibility of second derivative
                     menuShow : Float, -- Visibility of menu
                     pos : (Float, Float), -- Position graph is dragged to
                     tooltipShow : Bool, -- Visibility of toolip
                     tooltipPos : (Float, Float), -- Coordinate to show tooltip for
                     accelHelp : Bool, -- Visibility of Help Menu
                     -- Holds current state 
                     velHelp : Bool,
                     state : State,
                     qDown : Bool,
                     eDown : Bool,
                     wDown : Bool,
                     aDown : Bool,
                     sDown : Bool,
                     dDown : Bool,
                     leftDown : Bool,
                     upDown : Bool,
                     rightDown : Bool,
                     downDown : Bool, 
                     --------------
                     -- Models for imported
                     menuModel : Joshin10.LandingPage1.Model,
                     docModel : Joshin10.Docs0.Model,
                     docShow :  Float,
                     mainShow : Float
                     }

update msg model = case msg of
                     Tick t (keys, (x, y), (a, w)) -> 
                       case keys (Key "e") of
                         Down -> 
                           { model | time = t, fdz = model.fdz + 0.1, eDown = True}
                         
                         otherwise -> case keys (Key "q") of
                                      Down -> { model | time = t, fdz = model.fdz - 0.1, qDown = True}
                                      _ ->
                                           { model | time = t, 
                                                     eDown = False,
                                                     qDown = False,
                                                     wDown = w == 1,
                                                     sDown = w == -1,
                                                     aDown = a == -1,
                                                     dDown = a == 1,
                                                     leftDown = x == -1,
                                                     rightDown = x == 1,
                                                     upDown = y == 1,
                                                     downDown = y == -1,
                                                 {-Keys are binded as follows:
                                                  1. W/S -> Vertical Shift,
                                                  2. A/D -> Horizontal Shift
                                                  3. Arrow Up/Down -> Stretch factor-}

                                                 fdy = model.fdy + w, 
                                                 fdx = model.fdx + a,
                                                 dx = model.dx - x,
                                                 dy = model.dy - y}
                     
                     -- Once mouse is lifted up, reset drag position to 0
                     LetGo -> {model | state = Waiting, pos = (0, 0)}  
                     
                     -- Toggles between light/dark mode
                     Theme -> {model | theme = not model.theme}
                     
                     --Checks for enter key press (keycode 13)
                     KeyDown x -> if x == 13 then
                                    { model | f = model.input }
                                  else
                                    model
                     -- Stores what currently typed in box (used as a buffer)                    
                     Input txt -> { model | input = String.filter (\x -> Char.isDigit x || x == '-' || (List.member x ['.',','] && (String.length model.input) > 0)) txt }
                     -- Does something but nothing which is something                    
                     Nothing -> model
                     
                     -- Shows first derivative
                     DxShow -> {model | dxShow = if model.dxShow == 1 then 0 else 1}
                     
                     --Shows second derivative
                     Dx2Show -> { model | dx2Show = if model.dx2Show == 1 then 0 else 1}
                     
                     -- Textbox
                     
                     TextShow -> {model | inputShow = terny (model.inputShow == 1) 0 1 }
                     
                     -- Tooltip
                     ToolTip pos -> {model | tooltipShow = not model.tooltipShow, tooltipPos = pos }
                     
                     --Toggles menu visibility
                     ToggleMenu -> {model | menuShow = if model.menuShow == 1 then 0 else 1}
                     
                     -- Accel 
                     ToggleAccelHelp -> {model | accelHelp = not model.accelHelp}
                 
                     -- Velocity 
                     ToggleVelHelp-> {model | velHelp = not model.velHelp}
                 
                     -- Handles dragged position
                     NewPos pos -> model
                     
                     -- to documentation page
                     Docs -> {model | docShow = 1, mainShow = 0}
                     
                     -- from docs/homepage to app
                     ToApp -> {model | docShow = 0, mainShow = 0}
                     -- Msg handled by imported modules
                     DocsMsg dmsg -> {model | docModel = Joshin10.Docs0.update dmsg model.docModel}
                     LandingMsg lmsg -> {model | menuModel = Joshin10.LandingPage1.update lmsg model.menuModel}
                   
 
 
sub_ (x,y) (u,v) = (x-u,y-v)
add_ (x,y) (u,v) = (x+u,y+v)

init = { time = 0, 
         dy = 0, 
         dx = 0, 
         fdx = 0, 
         fdy = 0, 
         fdz = 1,
         theme = True, 
         f = "1,1", 
         input = "", 
         inputShow = 1,
         dxShow = 0, 
         dx2Show = 0,
         menuShow = 1, 
         pos = (0,0),
         tooltipShow = False, 
         tooltipPos = (0, 0),
         accelHelp = False,
         velHelp = False,
         state = Waiting,
         qDown = False,
         eDown = False,
         wDown = False,
         aDown = False,
         sDown = False,
         dDown = False,
         leftDown = False,
         upDown = False,
         rightDown = False,
         downDown = False,
         menuModel = Joshin10.LandingPage1.init,
         docModel = Joshin10.Docs0.init,
         docShow = 0,
         mainShow = 1}

main = gameApp Tick { model = init, view = view, update = update, title = "Physualiser" }

view model = collage 192 128 (myShapes model)



----Latex util

type Expr
  = EmptyExpr -- useful for matrices
  | ExprVar Char
  | ConstFloat Float
  | ConstFrac Int Int
  | ConstInt Int
  | ExprTuple (List Expr)
  | SuperSub Expr (List Expr) (List Expr)
  | FunApp Model FunName (List Expr)

type SumSymbol = Sum | Product 

type alias FunName = String

type alias Boxed = ((Float,Float,Float),Shape Msg)

emptyBox = ((0,0,0),group [])

type alias FunDict = Dict FunName ((Bool,Int),List Expr -> Boxed)

xExpr = ExprVar 'x'
yExpr = ExprVar 'y'


showBox ((w,h,d),sh) = sh

normalHeight = 9
normalWidth = 7

prettyExpr : Model -> Expr -> Boxed
prettyExpr model expr =
  case expr of
    EmptyExpr -> 
      ( (0, normalHeight, 0) 
        , group []
        )
    ExprVar char -> 
      ( (normalWidth, normalHeight, 0) 
        , text (String.fromChar char) |> italic |> filled (theme model)
        )
    ExprTuple exprs -> join model boxedComma (List.map (prettyExpr model) exprs) |> parens Round
        
    ConstInt i ->
      let 
        digits = String.fromInt i
        width = normalWidth * (String.length digits |> toFloat)
      in
        ((width,normalHeight,0),text digits |> filled (theme model))
    
    FunApp m funName args ->
      case Dict.get funName (funDict m) of
        Just (needsParens,drawFun) -> drawFun args 
        _ ->
          if String.length funName == 1 then
            (join model boxedComma (List.map (prettyExpr model) args) |> parens Round)
              |> prepend model
                  ((normalWidth,normalHeight,0),text funName |> italic |> filled (theme model))
          else
            unfinished "multi-character function"
   
    ConstFrac i j ->
      let 
        numerator = String.fromInt i
        wNum = normalWidth * (String.length numerator |> toFloat) * 0.75
        denominator = String.fromInt j
        wDen = normalWidth * (String.length denominator |> toFloat) * 0.75
        width = wNum + wDen + 0.375 * normalWidth
      in
        ((width,normalHeight,0)
        , [ text numerator |> filled (theme model) |> scale 0.75 |> move (0,0.25 * normalHeight)
          , text "/" |> filled (theme model) |> move (wNum - 0.125 * normalWidth,0 * normalHeight)
          , text denominator |> filled (theme model) |> scale 0.75 |> move (wNum + 0.375 * normalWidth,0)
          ] 
          |> group
        )
        
    SuperSub baseExpr superscripts subscripts ->
      let
        ((wSuper,hSuper,dSuper),super) = join model boxedComma (List.map (prettyExpr model) superscripts) 
                                      |> scaleBox 0.75
        ((wSub,hSub,dSub),sub) = join model boxedComma (List.map (prettyExpr model) subscripts) 
                                      |> scaleBox 0.75
        ((wBase,hBase,dBase),base) = prettyExpr model baseExpr
      in
        ( (wBase + max wSuper wSub, normalHeight, max dSub (-0.4*hBase)) 
        , [ base
          , super |> move (wBase,0.4*hBase)
          , sub |> move (wBase,-0.4*hBase)
          ]  |> group
        )
        
    ConstFloat f ->
      let 
        digits = String.fromFloat f
        width = normalWidth * (String.length digits |> toFloat)
      in
        ((width,normalHeight,0),text digits |> filled (theme model))
      
            
-- symbol ∑ ∏ ∫ ⋃ ⋂, upper, lower limit, body (includes dt)

scaleBox s ((w,h,d),sh) = ((w*s,h*s,d*s),sh |> scale s)
centerIn (fullWidth,fullHeight) ((w,h,d),sh) =  ((fullWidth,fullHeight,d)
                                                ,sh |> move (0.5*(fullWidth - w)
                                                            ,0.5*(fullHeight - h))
                                                )

zero = ConstInt 0
one = ConstInt 1
plus x y model = FunApp model "+" [x,y]
minus x y model = FunApp model "－" [x,y]
times x y model = FunApp model "" [x,y]

blueBox = ((10,10,0),square 10 |> outlined (solid 0.5) (rgb 0 0 255) |> move (5,5))
redBox = ((10,10,0),square 10 |> outlined (solid 0.5) (rgb 255 0 0) |> move (5,5))

funDict model = Dict.fromList (functions model )
functions model =
  [("+",(True,mkInfix model ((1.4*normalWidth,normalHeight,0),text "+" |> filled (theme model) |> move (1,0))))
  ,("－",(True,mkInfix  model ((1.4*normalWidth,normalHeight,0),text "－" |> filled (theme model) |> scale 1.5 |> move (1.25,-1.5))))
  ,("⨉",(True,mkInfix  model ((1.4*normalWidth,normalHeight,0),text "⨉" |> filled (theme model) |> scale 0.9 |> move (0.5,0))))
  ,("/",(True,mkInfix  model ((1.4*normalWidth,normalHeight,0),text "/" |> filled (theme model) |> move (2.5,0))))
  ,(" ",(True,mkInfix model  ((0.1*normalWidth,normalHeight,0),group [])))
  ,("",(True,mkInfix  model ((0,normalHeight,0),group [])))
  ]

boxedComma = ((normalWidth,normalHeight,0),text "," |> filled white)
space = ((normalWidth,normalHeight,0),group [])
thinSpace = ((0.5*normalWidth,normalHeight,0),group [])

mkOp : Model -> Int -> (Float,String) -> List Expr -> Boxed
mkOp model numArgs (width,name) exprs =
  if List.length exprs /= numArgs then 
    unfinished <| String.concat ["args do not match for ", name]
  else
    let 
      subExprs = List.map (prettyExpr model) exprs
    in
      (join model boxedComma subExprs |> parens Round)
        |> prepend model
            ((width * normalWidth,normalHeight,0),text name |> filled (theme model))
            
mkPow : Model -> List Expr -> List Expr -> Boxed
mkPow model powExprs baseExprs =
  case (powExprs,baseExprs) of
    ([ConstFrac 1 degree],[baseExpr]) ->
      let 
        ((wBase,hBase,dBase),base) = prettyExpr model baseExpr
        ((wDeg,hDeg,_),deg) = prettyExpr model (ConstInt degree) |> scaleBox 0.5
      in
        ( ( -0.73 * normalHeight * 0.01 * hBase + wBase + normalHeight * 0.01 * 40
          , hBase
          , dBase)
        , [ root (wBase * 100 / hBase) |> filled (theme model) |> scale ( hBase * 0.01 ) |> move ( 0.73 * normalHeight * 0.01 * hBase, 0.25 * normalHeight)
          , deg |> move (-5,4)
          , base |> move ( 0.73 * normalHeight * 0.01 * hBase, 0) ]
            |> group
            |> move ( normalHeight * 0.01 * 40 , 0)
        )
    ([powExpr],[baseExpr]) ->
      let
        ((wBase,hBase,dBase),base) = prettyExpr model baseExpr |> parens Round
        ((wDeg,hDeg,_),deg) = prettyExpr model powExpr |> scaleBox 0.75
      in
        ( (wBase + wDeg, hBase + 0.5 * hDeg,dBase )
        , [base, deg |> move (wBase,0.5*hBase)] |> group )
    _ ->
      unfinished "mkPow got multiple exponents"
    
root width = polygon [(0,60)
               ,(-37,-35.12)
               ,(-56,5.2896)
               ,(-73.35,-6.338)
               ,(-71.54,-9.769)
               ,(-60.49,-3.192)
               ,(-40.571,-50)
               ,(-39.3,-50)
               ,(2.603,57)
               ,(width-1,57)
               ,(width,60)
               ,(0,60)]


join model inter boxes =
  case boxes of
    [] -> ((0,0,0),group [])
    [b1] -> b1
    b1::b2::more -> prepend model ( prepend model b1 inter ) ( join model inter (b2::more) )

prepend model ((w0,h0,d0),sh0) ((w1,h1,d1),sh1) =
  ((w0+w1,max h0 h1,max d0 d1),group [sh0, sh1 |> move (w0,0)])
  
placeRelative : (Float,Float) -> Boxed -> Boxed -> Boxed
placeRelative (x,y) ((w0,h0,d0),sh0) ((w1,h1,d1),sh1) =
  let
    (x0,x1,w) = if x < 0 then 
                  (0,-x,max (w1-x) w0) 
                else 
                  (x,0,max w1 (w0+x))
    (y0,y1,h) = if y < 0 then 
                    (0,-y,max (h1-y) h0) 
                  else 
                    (y,0,max h1 (h0+y))
    d = max d1 (d0-y)
  in
  ( (w,h,d)
  , group [ sh0 |> move (x0,y0)
          , sh1 |> move (x1,y1)
          ])

placeUnderWithNudge (x,y) ((w0,h0,d0),sh0) ((w1,h1,d1),sh1) =
  placeRelative (x, y - h0) ((w0,h0,d0),sh0) ((w1,h1,d1),sh1)

placeDescendedWithNudge (x,y) ((w0,h0,d0),sh0) ((w1,h1,d1),sh1) =
  let
    (x0,x1,w) = if x < 0 then 
                  (0,-x,max (w1-x) w0) 
                else 
                  (x,0,max w1 (w0+x))
  in
  ( (w,h1,max d1 (h0 - y + d0))
  , group [ sh0 |> move (x0,y - h0)
          , sh1 |> move (x1,0)
          ])

placeOverWithNudge (x,y) ((w0,h0,d0),sh0) ((w1,h1,d1),sh1) =
  placeRelative (x, y + h1) ((w0,h0,d0),sh0) ((w1,h1,d1),sh1)

unfinished : String -> Boxed
unfinished txt = ((20,20,0),text txt |> filled red)

type ParenType 
  = Round
  | Square
  | OpenClosed
  | ClosedOpen
  | Curly
  | Angle
  
mkInfix model boxedOp args = 
  case args of
    [left,right] ->
      prepend model (prettyExpr model left) 
        <| prepend model boxedOp (prettyExpr model right)
    _ -> unfinished "mkInfix needs 2 args"
      
parens : ParenType -> Boxed -> Boxed
parens pType ((width,height,descender),shape) = 
  case pType of
      Round ->
        ( (2*roundWidth * (height * 0.01) * (1 - height * 0.01) + 0.4 * normalWidth + width, height, descender)
        , [ shape
              |> move (roundWidth * (height * 0.01) * (1 - height * 0.01) + 0.2 * normalWidth, 0)] |> group
        )
      _ -> unfinished "parens" 

roundWidth = 27.75
leftRound = curve (1.362,50)  [Pull (-27.75,30) (-27.75,0)
                              ,Pull (-27.75,-30) (1.362,-50)
                              ,Pull (1.362,-50) (2.4,-48.5)
                              ,Pull (-18,-34.00) (-18,-1.529)
                              ,Pull (-18,34) (2.4,48.5)
                              ,Pull (1.362,50) (1.362,50)]