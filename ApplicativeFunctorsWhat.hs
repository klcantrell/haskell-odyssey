main = do
  print $ pure (+) <*> (+3) <*> (*3) $ 3

  print $ (\_ -> (+)) <*> (+3) <*> (*3) $ 3

  print $ fmap (+) (+3) <*> (*3) $ 3

  print $ (+) . (+3) <*> (*3) $ 3

  print $ 
    (\y -> 
      (\x -> (\_ -> (+)) x ((+3) x)) y
      ((*3) y)) 
        $ 3