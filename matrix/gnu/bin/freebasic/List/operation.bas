#lang "fblite"
declare function rpminfo(byref rpm as string, byref setting as string, _
 byref verb as string) as string

type rpm 
    setting as string
    verb as string
end type 

dim mps as string

end 
