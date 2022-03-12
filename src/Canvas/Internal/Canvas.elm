module Canvas.Internal.Canvas exposing
    ( DrawOp(..)
    , Drawable(..)
    , PathSegment(..)
    , Point
    , Renderable(..)
    , Setting(..)
    , Shape(..)
    , Text
    )

import Canvas.Internal.CustomElementJsonApi exposing (Commands)
import Canvas.Internal.Other exposing (WorldCoordinates)
import Canvas.Texture exposing (Texture)
import Circle2d exposing (Circle2d)
import Color exposing (Color)
import Length exposing (Meters)
import Point2d exposing (Point2d)
import Rectangle2d exposing (Rectangle2d)


type alias Point =
    Point2d Meters WorldCoordinates


type Setting
    = SettingCommand Canvas.Internal.CustomElementJsonApi.Command
    | SettingCommands Canvas.Internal.CustomElementJsonApi.Commands
    | SettingDrawOp DrawOp
    | SettingUpdateDrawable (Drawable -> Drawable)


type DrawOp
    = NotSpecified
    | Fill Color
    | Stroke Color
    | FillAndStroke Color Color


type Drawable
    = DrawableText Text
    | DrawableShapes (List Shape)
    | DrawableTexture Point Texture
    | DrawableClear Point Float Float
    | DrawableClearScreen
    | DrawableGroup (List Renderable)


type Renderable
    = Renderable
        { commands : Commands
        , drawOp : DrawOp
        , drawable : Drawable
        }


type alias Text =
    { maxWidth : Maybe Float, point : Point, text : String }


type Shape
    = Rect (Rectangle2d Meters WorldCoordinates)
    | Circle (Circle2d Meters WorldCoordinates)
    | Path Point (List PathSegment)
    | Arc Point Float Float Float Bool


type PathSegment
    = ArcTo Point Point Float
    | BezierCurveTo Point Point Point
    | LineTo Point
    | MoveTo Point
    | QuadraticCurveTo Point Point
