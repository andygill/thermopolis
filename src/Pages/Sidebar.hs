
data Sidebar = TopLevel 
        


welcomePage :: ContentReader f => [Sidebar] -> f Page
welcomePage = do
 readPage "sidebar.html" []
