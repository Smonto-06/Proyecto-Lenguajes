import Data.Time.Clock
import Data.List
import System.IO
import Control.Exception

-- Definición del tipo de datos para representar la información de un estudiante
data Estudiante = Estudiante {
    idEstudiante :: String,
    entrada :: UTCTime,
    salida :: Maybe UTCTime
} deriving (Show, Read)

-- Función para registrar la entrada de un estudiante
registrarEntrada :: String -> UTCTime -> [Estudiante] -> [Estudiante]
registrarEntrada id tiempo estudiantes =
    Estudiante id tiempo Nothing : estudiantes

-- Función para registrar la salida de un estudiante
registrarSalida :: String -> UTCTime -> [Estudiante] -> [Estudiante]
registrarSalida id tiempo = map (\e -> if id == idEstudiante e then e { salida = Just tiempo } else e)


-- Función para buscar un estudiante por su ID
buscarEstudiante :: String -> [Estudiante] -> Maybe Estudiante
buscarEstudiante id = find (\e -> id == idEstudiante e && isNothing (salida e))

    where
        isNothing Nothing = True
        isNothing _       = False

-- Función para calcular el tiempo en la universidad
tiempoEnUniversidad :: Estudiante -> IO NominalDiffTime
tiempoEnUniversidad estudiante = do
    tiempoActual <- getCurrentTime
    return $ diffUTCTime tiempoActual (entrada estudiante)

-- Función para guardar los estudiantes en un archivo
guardarEstudiantes :: [Estudiante] -> IO ()
guardarEstudiantes estudiantes = do
    withFile "universidad.txt" WriteMode $ \h -> do
        hPutStr h (unlines (map show estudiantes))
    putStrLn "Datos guardados en universidad.txt."

-- Función para cargar los estudiantes desde un archivo sin usar deepseq
cargarEstudiantes :: IO [Estudiante]
cargarEstudiantes = do
    contenido <- readFile "universidad.txt"
    let lineas = lines contenido
    if null lineas
        then return []  -- Si el archivo está vacío, devolver una lista vacía
        else length lineas `seq` return (map read lineas)

-- Función para listar los estudiantes en la universidad
listarEstudiantes :: [Estudiante] -> IO ()
listarEstudiantes [] = putStrLn "No hay estudiantes registrados."
listarEstudiantes estudiantes = do
    putStrLn "Estudiantes en la universidad:"
    mapM_ print estudiantes

-- Menú principal
main :: IO ()
main = do
    putStrLn "Iniciando programa..."
    estudiantes <- cargarEstudiantes
    putStrLn "Datos cargados correctamente."
    putStrLn "¡Bienvenido al Sistema de Registro de Estudiantes!"
    cicloPrincipal estudiantes

cicloPrincipal :: [Estudiante] -> IO ()
cicloPrincipal estudiantes = do
    putStrLn "\nSeleccione una opción:"
    putStrLn "1. Registrar entrada de estudiante"
    putStrLn "2. Registrar salida de estudiante"
    putStrLn "3. Buscar estudiante por ID"
    putStrLn "4. Listar estudiantes"
    putStrLn "5. Salir"

    opcion <- getLine
    case opcion of
        "1" -> do
            putStrLn "Ingrese el ID del estudiante:"
            idEst <- getLine
            tiempoActual <- getCurrentTime
            let estudiantesActualizados = registrarEntrada idEst tiempoActual estudiantes
            putStrLn $ "Estudiante con ID " ++ idEst ++ " registrado."
            guardarEstudiantes estudiantesActualizados
            cicloPrincipal estudiantesActualizados
        
        "2" -> do
            putStrLn "Ingrese el ID del estudiante que sale:"
            idEst <- getLine
            estudiantesActualizados <- cargarEstudiantes  
            let estudianteEncontrado = any (\e -> idEst == idEstudiante e) estudiantesActualizados
            if estudianteEncontrado
                then do
                    tiempoActual <- getCurrentTime
                    let nuevosEstudiantes = registrarSalida idEst tiempoActual estudiantesActualizados
                    putStrLn $ "Estudiante con ID " ++ idEst ++ " ha salido."
                    guardarEstudiantes nuevosEstudiantes
                    cicloPrincipal nuevosEstudiantes
                else do
                    putStrLn "Error: No se encontró un estudiante con ese ID en el archivo."
                    cicloPrincipal estudiantes
        "3" -> do
            putStrLn "Ingrese el ID del estudiante a buscar:"
            idEst <- getLine
            case buscarEstudiante idEst estudiantes of
                Just estudiante -> do
                    tiempoTotal <- tiempoEnUniversidad estudiante
                    putStrLn $ "El estudiante con ID " ++ idEst ++ " está en la universidad."
                    putStrLn $ "Tiempo en la universidad: " ++ show tiempoTotal ++ " segundos."
                Nothing -> putStrLn "Estudiante no encontrado."
            cicloPrincipal estudiantes
        
        "4" -> do
            listarEstudiantes estudiantes
            cicloPrincipal estudiantes
        
        "5" -> putStrLn "¡Hasta luego!"
        
        _ -> do
            putStrLn "Opción no válida. Por favor, seleccione una opción válida."
            cicloPrincipal estudiantes
