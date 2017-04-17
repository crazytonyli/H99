ctl_last :: [a] -> a
ctl_last [x] = x
ctl_last (_:xs) = ctl_last xs
