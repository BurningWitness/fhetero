module Data.Type.Deep where



data Depth = S *
           | D [Depth]
