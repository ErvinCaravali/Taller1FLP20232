
# Guía de Uso de Git

Esta guía te ayudará a empezar con Git y a aprender los conceptos básicos para administrar tu código fuente.

## Table of Contents

1. [Instalación](#instalación)
2. [Configuración Inicial](#configuración-inicial)
3. [Comandos Básicos](#comandos-básicos)
4. [Flujo de Trabajo Básico](#flujo-de-trabajo-básico)
5. [Ramas (Branches)](#ramas-branches)
6. [Colaboración](#colaboración)
7. [Resolución de Conflictos](#resolución-de-conflictos)
8. [Extras](#extras)

## Instalación

Descarga e instala Git desde [https://git-scm.com/](https://git-scm.com/).

## Configuración Inicial

Antes de comenzar, configura tu nombre de usuario y dirección de correo electrónico correctamente:


git config --global user.name "Tu Nombre"
git config --global user.email "tu@email.com"

## Comandos Básicos

- `git init`: Inicia un nuevo repositorio local.
- `git add <archivo>`: Añade un archivo al área de preparación.
- `git commit -m "Mensaje de commit"`: Crea un nuevo commit con los archivos en el área de preparación.
- `git status`: Muestra el estado actual del repositorio.
- `git log`: Muestra el historial de commits.

## Flujo de Trabajo Básico

1. Modifica archivos en tu directorio de trabajo.
2. Usa `git add <archivo>` para añadir al área de preparación.
3. Utiliza `git commit -m "Mensaje de commit"` para crear un nuevo commit.

## Ramas (Branches)

- `git branch`: Lista todas las ramas locales.
- `git branch <nombre>`: Crea una nueva rama.
- `git checkout <nombre>`: Cambia a una rama específica.
- `git merge <nombre>`: Combina la rama especificada con la actual.

## Colaboración

1. Clona un repositorio remoto: `git clone <URL>`.
2. Añade un nuevo repositorio remoto: `git remote add origin <URL>`.
3. Descarga los cambios del repositorio remoto: `git pull origin <rama>`.
4. Sube tus cambios al repositorio remoto: `git push origin <rama>`.

## Resolución de Conflictos

Si hay conflictos al fusionar ramas:

1. Abre los archivos con conflictos.
2. Edita los archivos para resolver los conflictos.
3. Añade los archivos resueltos: `git add <archivo>`.
4. Crea un nuevo commit: `git commit -m "Mensaje de commit"`.

## Extras

- `.gitignore`: Archivo para ignorar ciertos archivos o directorios.
- `.gitattributes`: Configuración adicional para el control de versiones.

¡Ahora estás listo para empezar a usar Git! Si necesitas más información, consulta la [documentación oficial de Git](https://git-scm.com/doc).
```

Este archivo `README.md` proporciona una introducción básica y una guía paso a paso para empezar a usar Git. Puedes personalizarlo o agregar más detalles según las necesidades de tu proyecto.
