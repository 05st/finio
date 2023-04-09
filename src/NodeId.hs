module NodeId where

import qualified Data.IntMap as IM

import Error.Diagnose

type NodeId = Int

-- Position (Int, Int) (Int, Int) FilePath
type PositionMap = IM.IntMap Position -- NodeId -> Position
