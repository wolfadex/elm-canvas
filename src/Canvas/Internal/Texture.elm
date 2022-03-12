module Canvas.Internal.Texture exposing
    ( Image
    , Source(..)
    , Sprite
    , Texture(..)
    , decodeImageLoadEvent
    , decodeTextureImage
    , drawTexture
    )

import Canvas.Internal.CustomElementJsonApi exposing (Commands)
import Json.Decode exposing (Decoder, Value)


type alias Image =
    { json : Value
    , width : Float
    , height : Float
    }


type Source msg
    = TSImageUrl String (Maybe Texture -> msg)


type Texture
    = TImage Image
    | TSprite Sprite Image


type alias Sprite =
    { x : Float, y : Float, width : Float, height : Float }


decodeImageLoadEvent : Decoder (Maybe Texture)
decodeImageLoadEvent =
    Json.Decode.field "target" decodeTextureImage


decodeTextureImage : Decoder (Maybe Texture)
decodeTextureImage =
    -- TODO: Verify the Value is actually a DOM image
    Json.Decode.value
        |> Json.Decode.andThen
            (\image ->
                Json.Decode.map3
                    (\tagName width height ->
                        if tagName == "IMG" then
                            Just
                                (TImage
                                    { json = image
                                    , width = width
                                    , height = height
                                    }
                                )

                        else
                            Nothing
                    )
                    (Json.Decode.field "tagName" Json.Decode.string)
                    (Json.Decode.field "width" Json.Decode.float)
                    (Json.Decode.field "height" Json.Decode.float)
            )


drawTexture : Float -> Float -> Texture -> Commands -> Commands
drawTexture x y t cmds =
    (case t of
        TImage image ->
            Canvas.Internal.CustomElementJsonApi.drawImage 0 0 image.width image.height x y image.width image.height image.json

        TSprite sprite image ->
            Canvas.Internal.CustomElementJsonApi.drawImage sprite.x sprite.y sprite.width sprite.height x y sprite.width sprite.height image.json
    )
        :: cmds
