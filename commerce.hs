--Delgado Marco 1591290

type Nombre = String
type Precio = Float
type Producto = (Nombre, Precio)

precioTotal :: Producto -> Float -> Float -> Float -> Float
precioTotal (_, precio) cantidad descuento costoEnvio = aplicarCostoDeEnvio (aplicarDescuento precio descuento * cantidad) costoEnvio

precioUnitario :: Producto -> Float -> Float -> Float
precioUnitario (_, precio) descuento costoEnvio = (aplicarCostoDeEnvio costoEnvio . aplicarDescuento descuento) precio

productoDeElite :: Producto -> Bool
productoDeElite producto = productoDeLujo producto && productoCodiciado producto && not (productoCorriente producto)

aplicarDescuento :: Float -> Float -> Float
aplicarDescuento descuento precio = precio * (1 - descuento)

entregaSencilla :: String -> Bool
entregaSencilla dia = (even . length) dia

descodiciarProducto :: Producto -> Producto
descodiciarProducto (nombre, precio) = (take 10 nombre, precio)

productoDeLujo :: Producto -> Bool
productoDeLujo (nombre, _) = elem 'x' nombre || elem 'z' nombre

aplicarCostoDeEnvio :: Float -> Float -> Float
aplicarCostoDeEnvio costoEnvio precio = precio + costoEnvio

productoCodiciado :: Producto -> Bool
productoCodiciado (nombre, _) = length nombre > 10

productoCorriente :: Producto -> Bool
productoCorriente (nombre, _) = (esVocal . head) nombre

esVocal :: Char -> Bool
esVocal letra = elem letra "aeiouáéíóúAEIOUÁÉÍÓÚ"

productoXL :: Producto -> Producto
productoXL (nombre, precio) = (nombre ++ "XL", precio)

versionBarata :: Producto -> Producto
versionBarata producto = ((reverse . fst . descodiciarProducto) producto, snd producto)


--take :: Int -> [a] -> [a]
--drop :: Int -> [a] -> [a]
--head :: [a] -> a
--elem :: Eq a => a -> [a] -> Bool
--reverse :: [a] -> [a]
