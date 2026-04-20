# 📁 Portafolio — Estadística Espacial
### William Yoel Incacutipa Incacutipa | Código: 215341
### Ing. Estadística e Informática · UNA Puno · SEM 2026-I

---

## 🚀 Cómo usar este portafolio

### Abrir localmente
1. Extrae el ZIP en cualquier carpeta de tu computadora
2. Haz doble clic en `index.html` → se abre directo en tu navegador
3. ¡Listo! No necesitas internet ni servidor

### Estructura de archivos

```
portfolio-espacial/
│
├── index.html          ← Página principal (abre esto)
├── css/
│   └── style.css       ← Estilos del portafolio
├── js/
│   └── main.js         ← Animaciones e interacciones
└── tareas/
    ├── tarea1/
    │   ├── informe.pdf  ← TU informe (cópialo aquí)
    │   └── codigo.R     ← TU código R (cópialo aquí)
    ├── tarea2/ ...
    ├── tarea3/ ...
    ├── tarea4/ ...
    ├── tarea5/ ...
    └── tarea6/ ...
```

---

## ✏️ Cómo agregar tus archivos

Para cada tarea, solo copia tus archivos a la carpeta correspondiente:

| Tarea | Carpeta | Archivos esperados |
|---|---|---|
| Tarea 1 | `tareas/tarea1/` | `informe.pdf`, `codigo.R` |
| Tarea 2 | `tareas/tarea2/` | `informe.pdf`, `codigo.R` |
| Tarea 3 | `tareas/tarea3/` | `informe.pdf`, `codigo.R` |
| Tarea 4 | `tareas/tarea4/` | `informe.pdf`, `codigo.R` |
| Tarea 5 | `tareas/tarea5/` | `informe.pdf`, `codigo.R` |
| Tarea 6 | `tareas/tarea6/` | `informe.pdf`, `codigo.R` |

---

## ⚙️ Personalización

Para cambiar datos del portafolio, edita `index.html` con cualquier editor de texto (Notepad, VS Code, etc.).

Para cambiar el porcentaje de progreso o las estadísticas, edita las primeras líneas de `js/main.js`:

```js
const PORTFOLIO_CONFIG = {
  totalTareas: 6,
  tareasEnviadas: 2,       // ← cambia esto
  tareasRevision: 1,
  tareasPendientes: 3,
  promedio: 17.5,          // ← y esto
  porcentajeProgreso: 38,  // ← y esto
};
```

---

## 🌐 Publicar en internet (opcional)

Para compartir con tu docente online:
- **GitHub Pages**: Sube la carpeta a un repositorio y activa GitHub Pages
- **Netlify**: Arrastra la carpeta a netlify.com/drop (gratis)
- **Vercel**: Conecta tu repositorio de GitHub

---

*Desarrollado para el curso Estadística Espacial · 2026*
