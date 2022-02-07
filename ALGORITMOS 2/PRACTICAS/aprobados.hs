aprobados [] = []
aprobados (x:xs) = if x > 6 then x : aprobados xs
                            else aprobados xs

