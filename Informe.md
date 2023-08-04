# Informe

## Contexto

Tower Scape es un juego de plataformas, donde un slime mago de hielo intenta escapar de una torre llena de enemigos y plataformas traicioneras. El objetivo es alcanzar la mayor puntuación posible y así tratar de evitar que el protagonista se vea "abrazado" por las llamas que lo persiguen.

## Tecnologías y Herramientas utilizadas

Para el desarrollo del juego se utilizó el lenguaje de programación Haskell, con el apoyo de los siguientes paquetes: `gloss`, `gloss-juicy` y `JuicyPixels`. Para el renderizado se utilizó `gloss`, para cargar las imágenes (personajes, plataformas, etc.) se utilizó `JuicyPixels` y `gloss-juicy` como enlace entre estos dos paquetes.

## Implementación

El proyecto está dividido en diferentes módulos por funcionalidades, buscando una mayor organización.

### Juego

En `Game.hs` se definen los factores fundamentales que intervienen en el juego: `Game`,`GameState`, `Player`, `Platform`, `PlatformType`, `EnemyType`, `EnemyState` y `Projectile`.

#### Game y GameState

El tipo `Game` representa el juego en sí. El juego puede encontrarse en uno de tres estados: Idle, Playing o GameOver, los mismos son representados con el tipo `gameState`.

En `Game` también se guarda `gamePlatforms`, función que devuelve una lista con las plataformas que se encuentran actualmente en el juego, también se tiene la cantidad de las mismas y una función `gameNeedPlatforms` que indica si es necesario generar nuevas plataformas.

Además, se tiene información referente al jugador, `gamePlayer` representa al jugador actual y `gamePlayerAxisX` que indica hacia que lado de la X se moverá el jugador.\
La función `gameProjectiles` devuelve una lista de `Projectile` que representa los proyectiles que están "en juego".

Luego se tiene otras funciones como `gameScore` para la puntuación del juego, `gameBestScores` que devuelve una lista con las puntuaciones. `gameButtons` que devuelve la lista de botones usados en el juego, `gameCameraPos` que representa la posición actual de la cámara del juego y `gameSeed`, la semilla actual para generar un random.

#### Jugador

El tipo `Player` representa a la entidad del jugador. De él es de interés guardar la posición actual y la velocidad actual, pues la misma puede variar. Esto se hace mediante las funciones `playerPosition` y `playerSpeed` respectivamente. El jugador tiene diferentes "estados": cayendo o saltando, vivo o muerto y un estado especial de inmunidad que es otorgado al hacer colisión con un tipo específico de plataformas. Para este fin fueron creadas las funciones `playerIsFalling`, `playerAlive` y `playerIsInmune`.

#### Plataformas

Las plataformas constituyen uno de los elementos más importantes del juego. Se definieron cuatro tipos de plataformas: Normal, Inestables, Enemigas y Plataforma Final.

 Las plataformas normales son las estándar que no tienen un comportamiento diferente al esperado.\
Las plataformas inestables son plataformas que están rotas y, en dependencia de la cantidad de veces que se caiga sobre ellas, se pueden romper completamente; tienen tres niveles diferentes de roturas, en ocasiones solo hace falta de un salto para que se quiebren, otras es necesario caer dos o tres veces. \
Las plataformas enemigas no deben ser tocadas bajo ningún concepto por el jugador, a menos que estén congeladas. Existen dos tipos de plataformas enemigas: plataformas de lava o un slime volador de fuego. Estos tipos están definidos en `enemyType`. Los enemigos pueden ser neutralizados gracias a los disparos de hielo del jugador (el protagonista es un mago de hielo). Se definen por tanto dos posibles estados para los enemigos: vivo o congelado, en `enemyState`. Si el jugador colisiona con la plataforma enemiga en estado congelado, entonces dicha plataforma tendrá el comportamiento de una plataforma normal, de otra manera el jugador caerá y el juego terminará.\
Se tiene un tipo de plataforma especial denominada `EndPlatform`, dicha plataforma solo aparece cuando el jugador pierde y representa un mar de lava que consume al jugador caído.

  En la implementación se define el tipo `Platform` con las siguientes funciones: `platformId` como identificador de las plataformas, `platformPosition` la posición actual de la plataforma, `platformType` el tipo de plataforma, `platformImpulse` es el impulso que la plataforma da al jugador, este valor puede variar en diferentes velocidades definidas y, dependiendo de la velocidad alcanzada, el jugador puede entrar en un estado de inmunidad en el salto, `platformEndMovement` y `platformStartMovement` que representan los puntos del movimiento de la plataforma, `platformSize` el tamaño de la plataforma y `platformDirection` que representa si la plataforma se mueve a la izquierda o la derecha.

#### Proyectiles

Los proyectiles son bolas de hielo que lanza el jugador y que, si colisionan con una plataforma enemiga, la misma se convertirá en una plataforma congelada y el jugador podrá hacer colisión con ella normalmente. Se define el tipo `Projectile` que contendrá un identificador del proyectil, la posición actual del proyectil y el movimiento del mismo.

### Renderizado

La función principal dentro de `Rendering.hs` es `gameAsPicture`, la cual recibe un `Game` y devuelve un `Picture` del juego. El renderizado en `gloss` funciona por capas, lo que significa que las capas de más arriba se superpondrán sobre las de abajo. Por ello se dibuja primero el fondo, luego las plataformas, luego los proyectiles, el jugador, los botones, la puntuación, los textos al comienzo y el final del juego, la plataforma final y los bordes. Cada uno de ellos se realiza en funciones separadas.

### Lógica

Al inicializar el juego, se generan las plataformas, se cargan los score y el juego se queda en el estado 'Idle' hasta que se presione la tecla "Space", donde el juego pasaría al estado 'Playing' que daría comienzo al juego.

En el desarrollo del juego intervienen una serie de funciones que son las que dan la funcionalidad deseada y que irán modificando al juego.

`poblate` es la función encargada de generar o purgar plataformas en el juego. Para generar plataformas se utiliza la función `generatePlatforms` verifica si es posible generar más plataformas en el juego, y si ese es el caso se llama a la función `generateOnePlatform` que es la encargada de generar la plataforma, donde su tipo y movimiento es determinado de forma "aleatoria". En caso de no poder generar otra plataforma, `generatePlatforms` devuelve el mismo juego. Para purgar las plataformas se utiliza la función `purgePlatforms` que verifica si dentro de la lista de plataformas actuales, exista una cuya posición esté fuera de los límites de la pantalla, de ser así significa que la plataforma ya no está en juego y por tanto es eliminada de la lista.

`moveProjectiles` maneja el movimiento de los proyectiles que son disparados, calcula la posición de los "nuevos proyectiles" en dependencia de su movimiento definido.

`projectilesCollision` detecta si un proyectil hace colisión con una plataforma enemiga, de ser así la plataforma enemiga pasa a ser parte de las plataformas congeladas.

Las funciones `movePlatform` y `movePlayer` se encargan de manejar todo lo relacionado al movimiento de las plataformas y el jugador respectivamente.

Para detectar colisiones entre las plataformas y el jugador se utiliza la función `detectCollisions`. Al final se verifica si determinados puntos de jugador se encuentran en el intervalo determinado por los bordes de la plataforma, en dependencia de por qué parte se desea que se realice la colisión.

Para el manejo de la puntuación se utiliza la función `updateScore`. La puntuación aumenta a medida que la aumenta la posición más alta alcanzada por el jugador.

`purgeProjectiles` es la función encargada de eliminar aquellos proyectiles que fueron disparados y que se encuentran fuera de los límites del ancho de la pantalla.

Por último, `updateCamera` actualiza la posición de la cámara para dar el efecto de que la misma sube una vez el jugador alcance una posición determinada.

Si el jugador hace colisión con una plataforma enemiga no congelada o cae fuera del límite inferior de la pantalla, entonces el juego entra en el estado de "Game Over".

En este estado se mostrará la puntuación alcanzada y la puntuación más alta que se ha tenido hasta el momento. Además aparecerá la plataforma Final que irá consumiendo al jugador. El juego se mantendrá en dicho estado hasta que se presione la tecla "Space", cayendo entonces en el estado "Idle".
