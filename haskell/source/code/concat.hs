(++) lst1 lst2 = case (lst1, lst2) of
                   ([], lst2) -> lst2
                   ((x:xs), lst2) -> x : (++) xs lst2
