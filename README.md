# Proyecto2-Lenguajes: Administrador Seguro de Contraseñas

Este proyecto es un **administrador de contraseñas de línea de comandos escrito en Haskell** como parte del curso de Lenguajes de Programación. Permite a los usuarios registrarse, iniciar sesión, y gestionar credenciales cifradas de forma segura.

---

##  Requisitos

- [Haskell Stack](https://docs.haskellstack.org/en/stable/README/)
- GHC >= 9.0 (Stack lo instalará automáticamente)

---

##  Instrucciones de uso

1. Clonar el repositorio o descarga los archivos del proyecto.

2. Desde la carpeta raíz del proyecto, compila con:

   ```bash
   stack build
   ```

3. Luego ejecuta el programa:

   ```bash
   stack exec admin
   ```

---

##  Funcionalidades

- Registro de usuarios (con validación de PIN)
- Inicio de sesión
- Agregar, modificar, eliminar y ver contraseñas
- Copiar usuario o contraseña al portapapeles
- Cifrado de datos utilizando una combinación de Base64 + cifrado tipo César

---

##  Estructura del proyecto

```
Proyecto2-Lenguajes/
├── src/
│   ├── /data               -- Carpeta donde se guarda la información de los usuarios
|   ├── Main.hs             -- Punto de entrada principal
│   ├── UI.hs               -- Interfaz en consola
│   ├── User.hs             -- Registro e inicio de sesión
│   ├── Password.hs         -- Gestión de contraseñas
│   ├── FileManager.hs      -- Lectura y escritura de archivos
│   ├── Crypto.hs           -- Funciones de cifrado/descifrado
│   └── Types.hs            -- Definición de tipos como PasswordEntry
├── Proyecto2-Lenguajes.cabal
├── README.md
├── Setup.hs
├── stack.yaml
└── stack.yaml.lock
```

---

##  Directorio de datos

Todas las credenciales y archivos temporales se almacenan en:

```
src/data/
```

Allí se crean:
- Un archivo `.txt` por usuario con sus contraseñas cifradas
- Un archivo temporal `current_user.tmp` para saber quién está logueado

---

##  Seguridad

Este programa usa una combinación de:

- Codificación en Base64
- Cifrado tipo César (desplazamiento de caracteres)
- Archivos separados por usuario
- Manejo de errores y validación de archivos corruptos
