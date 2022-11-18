# The Parser

## Pipeline

Source text: String 
   -> List PrimitiveBlock       -- via PrimitiveBlock.parse
   -> Forest PrimitiveBlock     -- via Parser.Tree.forestFromBlocks
   -> Forest ExpressionBlock    -- via Forest.map (BlockUtil.toExpressionBlock ... )