MPlayer Mode:
=============

Este es un minor-mode sencillo para controlar Mplayer desde Emacs. Fue motivado cuando traté de tomar notas mientras veía un video, lo pausaba contínuamente, lo atrasaba un poco, etc. Al final también me hubiera gustado insertar estampas de tiempo.

Así que esta es la solución (escribirlo probablemente tomó más tiempo que el que me hubiera ahorrado usándolo, pero ya veremos).

Para empezar, se debe cargar el archivo directamente:

	; Mplayer-mode
	(add-to-list 'load-path "~/.emacs.d/mplayer-mode/mplayer-mode.el")

o agregarlo en la ruta de carga (`load-path`) y llamar `(require 'mplayer)`.

	; Mplayer-mode
	(add-to-list 'load-path "~/.emacs.d/mplayer-mode/")
	(require 'mplayer-mode)

Ahora, si deseas tomar notas, llama `M-x mplayer-find-file`, esto inicia la reproducción del video.

Consulta la documentación para más información acerca de los demás comandos.

Es posible ajustar el formato de las estampas de tiempo.
