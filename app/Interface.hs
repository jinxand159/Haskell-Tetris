module Main where
-- import Graphics.Rendering.Cairo      
import Graphics.UI.Gtk
import Text.Read (readMaybe, read)    
 
import Tetris (tetrisMain)      

main :: IO ()  
main = do
    -- Inicializa GTK  
    _ <- initGUI
    -- Crea una nueva ventana 
    window <- windowNew 
    set window [ windowTitle := "Tetris"]  
    windowMaximize window
    widgetModifyBg window StateNormal (Color (63 * 257) (36 * 257) (99 * 257))

    -- Crea un botón
    buttonPlay <- buttonNewWithLabel "JUGAR"
    widgetSetSizeRequest buttonPlay 150 150
    _ <- onClicked buttonPlay $ do
        widgetDestroy window 
        tetrisMain 1
    -- Obtiene el Label del botón y cambia el tamaño de la fuente
    maybeLabel <- binGetChild buttonPlay
    case maybeLabel of
        Nothing -> return () 
        Just label -> do
            let labelWidget = castToLabel label
            fontButton <- fontDescriptionFromString "Monospace 30" 
            widgetModifyFont labelWidget (Just fontButton)
    -- Crea un botón
    buttonLevel <- buttonNewWithLabel "NIVELES"
    widgetSetSizeRequest buttonLevel 50 50
    _ <- onClicked buttonLevel $ do
        widgetDestroy window
        windowLevel
    -- Obtiene el Label del botón y cambia el tamaño de la fuente
    maybeLabel <- binGetChild buttonLevel
    case maybeLabel of
        Nothing -> return ()
        Just label -> do
            let labelWidget = castToLabel label
            fontButton <- fontDescriptionFromString "Monospace 15"
            widgetModifyFont labelWidget (Just fontButton)

    -- Crear titulos y subtitulo    
    title <- labelNew (Just "TETRIS")
    fontTitle <- fontDescriptionFromString "Monospace 50"   
    widgetModifyFont title (Just fontTitle)
    widgetModifyFg title StateNormal (Color (255 * 257) (255 * 257) (255 * 257))

    subtitle1 <- labelNew (Just "¡El clásico juego de bloques en Haskell!")
    fontSubtitle1 <- fontDescriptionFromString "Monospace 12"
    widgetModifyFont subtitle1 (Just fontSubtitle1)
    widgetModifyFg subtitle1 StateNormal (Color (255 * 257) (255 * 257) (255 * 257))
    
    subtitle2 <- labelNew (Just "Realizado por Jinxiang, José e Inés") 
    fontSubtitle2 <- fontDescriptionFromString "Monospace 10" 
    widgetModifyFont subtitle2 (Just fontSubtitle2)  
    widgetModifyFg subtitle2 StateNormal (Color 65535 65535 65535)  -- Cambia el color del texto a blanco

    -- Crea un Box para centrar el botón y el título  
    box <- vBoxNew False 0  
    -- Establece el tamaño de subtitle 
    boxPackStart box subtitle1 PackRepel 10 
    boxPackStart box title PackNatural 100
    boxPackStart box buttonPlay PackNatural 50
    boxPackStart box buttonLevel PackNatural 20
    boxPackStart box subtitle2 PackNatural 10
    -- Añade el Box a la ventana
    containerAdd window box    

    -- Muestra la ventana
    widgetShowAll window

    -- Cierra la aplicación cuando se cierra la ventana
    _ <- on window objectDestroy mainQuit 

    -- Comienza el bucle principal de GTK
    mainGUI

windowLevel :: IO ()
windowLevel = do
    _ <- initGUI
    -- Crea una nueva ventana
    windowLevel <- windowNew
    set windowLevel [ windowTitle := "Tetris"]  
    windowMaximize windowLevel
    widgetModifyBg windowLevel StateNormal (Color (63 * 257) (36 * 257) (99 * 257))

    -- Crear titulos y subtitulo   
    title <- labelNew (Just "NIVELES")
    fontTitle <- fontDescriptionFromString "Monospace 50"
    widgetModifyFont title (Just fontTitle)   
    widgetModifyFg title StateNormal (Color (255 * 257) (255 * 257) (255 * 257))

    subtitle1 <- labelNew (Just "Selecciona un nivel entre el 1 y el 9 \npara equilibrar a dificultad del juego \n\n1-2. Homer en un día normal en la oficina\n3-4. Frodo yendo a buscar el anillo, lento pero va\n5-6. Batman sin su batmovil\n7-8. Spiderman balanceandose por las calles de Brooklyn\n9. Más rápido que el rayo de Zeus")
    fontSubtitle1 <- fontDescriptionFromString "Monospace 12"
    widgetModifyFont subtitle1 (Just fontSubtitle1)
    widgetModifyFg subtitle1 StateNormal (Color (255 * 257) (255 * 257) (255 * 257))

    -- Crea un Entry para la entrada del usuario
    entry <- entryNew
    entrySetMaxLength entry 1  -- Limita la entrada a un solo carácter
    entrySetText entry "1"  -- Establece un valor inicial
    
    errorMessage <- labelNew (Just "Por favor, ingrese un número entre 1 y 9.")
    fonterrorMessage <- fontDescriptionFromString "Monospace 12"
    widgetModifyFont errorMessage (Just fonterrorMessage)
    widgetModifyFg errorMessage StateNormal (Color (255 * 257) (255 * 257) (255 * 257))

    -- Crea un botón para capturar la entrada del usuario
    button <- buttonNewWithLabel "Aceptar"
    widgetSetSizeRequest button 50 50
    _ <- onClicked button $ do
        userInput <- entryGetText entry
        widgetDestroy windowLevel
        case readMaybe userInput :: Maybe Int of
            Just n | n >= 1 && n <= 9 -> putStrLn $ "El usuario ingresó: " ++ show n
            _ -> putStrLn "Por favor, ingrese un número válido entre 1 y 9."
        tetrisMain (read userInput :: Int )
    -- Obtiene el Label del botón y cambia el tamaño de la fuente
    maybeLabel <- binGetChild button
    case maybeLabel of 
        Nothing -> return ()
        Just label -> do
            let labelWidget = castToLabel label
            fontButton <- fontDescriptionFromString "Monospace 15"
            widgetModifyFont labelWidget (Just fontButton)
    
    -- Crea un Box para centrar el botón y el título  
    box <- vBoxNew False 0  
    -- Establece el tamaño de subtitle 
    boxPackStart box title PackNatural 100
    boxPackStart box subtitle1 PackNatural 20  
    boxPackStart box entry PackNatural 10
    boxPackStart box button PackNatural 10  
    boxPackStart box errorMessage PackNatural 20
    -- Añade el Box a la ventana
    containerAdd windowLevel box    
    -- Muestra la ventana
    widgetShowAll windowLevel
    -- Cierra la aplicación cuando se cierra la ventana
    _ <- on windowLevel objectDestroy mainQuit
    mainGUI