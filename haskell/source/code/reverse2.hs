myReverse xs = rev xs []
  where rev lst reversed = case (lst, reversed) of
          ([], reversed) -> reversed
          ((x:xs), reversed) -> rev xs (x:reversed) 
