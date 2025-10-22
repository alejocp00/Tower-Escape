# Tower Escape

**Estudiantes:**

- Alejandro Camacho Pérez C-312: [GitHub](https://github.com/alejocp00/) [Telegram](https://t.me/alejocp00)
- Diana Laura Pérez Trujillo C-312: [GitHub](https://github.com/Kitsulee/) [Telegram](https://t.me/kitsu_lee)

**Propuesta:**

- Juego de plataforma al estilo [Doodle Jump](https://en.wikipedia.org/wiki/Doodle_Jump).

**Tecnologías:**

- Haskell:
  - Haskell permite expresar todos los comportamientos del juego de una forma clara mediante el lenguaje funcional.
  - Contiene librerías que hacen de intermediario entre OpenGL para el desarrollo de aplicaciones 2D. Específicamente usaremos [gloss](https://hackage.haskell.org/package/gloss), [aspecs-gloss](https://hackage.haskell.org/package/apecs-gloss) y de ser necesario [aspecs-physics](https://hackage.haskell.org/package/apecs-physics).

**Detalles:**

- Todos los niveles del juego serán generados de forma aleatoria.
- Existirán plataformas con distintos efectos que influirán en la dificultad del nivel.
- Se llevará un conteo de puntuación.

**Dependencias:**

El juego utiliza glut y aplay. En caso de que no lo pueda ejecutar, instale las dependencias con:

```bash
# Linux
sudo apt-get install freeglut3 libasound2

# Windows (usando MSYS2)
pacman -S mingw-w64-x86_64-freeglut
```

**Tutoriales:** [Español](Tutorial.es.md) [Inglés](Tutorial.en.md)
