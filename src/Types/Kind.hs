module Types.Kind where

data Kind
    = KindFunc Kind Kind
    | KindSeq
    | KindStar
    | KindAny
    deriving(Eq)

pshow_kind :: Kind -> String
pshow_kind f@(KindFunc _ _) = "(" ++ show f ++ ")"
pshow_kind x = show x

instance Show Kind where
    show (KindFunc a b) = pshow_kind a ++ "->" ++ show b
    show KindSeq = "Seq"
    show KindStar = "*"
    show KindAny = "@"