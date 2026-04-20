/* ═══════════════════════════════════════════════════════════
   PORTFOLIO — ESTADÍSTICA ESPACIAL
   main.js — Interacciones y dinámicas
═══════════════════════════════════════════════════════════ */

/* ── Configuración de tareas (editar aquí para actualizar) ── */
const PORTFOLIO_CONFIG = {
  totalTareas: 6,
  tareasEnviadas: 2,
  tareasRevision: 1,
  tareasPendientes: 3,
  promedio: 17.5,
  porcentajeProgreso: 38,
};

/* ── Progress bar animada ── */
function animateProgress() {
  const bar   = document.getElementById('pbar');
  const label = document.getElementById('pval');
  if (!bar || !label) return;

  const target = PORTFOLIO_CONFIG.porcentajeProgreso;

  // Pequeño retardo para que la transición CSS sea visible
  setTimeout(() => {
    bar.style.width = target + '%';
  }, 250);

  // Contador numérico
  let current = 0;
  const step  = Math.ceil(target / 60);
  const timer = setInterval(() => {
    current = Math.min(current + step, target);
    label.textContent = current + '%';
    if (current >= target) clearInterval(timer);
  }, 1400 / 60);
}

/* ── Toggle expand/collapse de tarjetas ── */
function toggleCard(card) {
  const isExpanded = card.classList.contains('expanded');

  // Colapsar todas primero
  document.querySelectorAll('.task-card.expanded')
    .forEach(c => c.classList.remove('expanded'));

  // Expandir la seleccionada si no estaba abierta
  if (!isExpanded) {
    card.classList.add('expanded');
    // Scroll suave si queda fuera de vista
    setTimeout(() => {
      const rect = card.getBoundingClientRect();
      if (rect.bottom > window.innerHeight - 40) {
        card.scrollIntoView({ behavior: 'smooth', block: 'nearest' });
      }
    }, 50);
  }
}

/* ── Teclado: Enter / Space para abrir tarjeta ── */
function handleKey(event, card) {
  if (event.key === 'Enter' || event.key === ' ') {
    event.preventDefault();
    toggleCard(card);
  }
}

/* ── Filtrar por unidad ── */
function filterUnit(unit, btn) {
  // Actualizar tab activo
  document.querySelectorAll('.nav-tab')
    .forEach(t => t.classList.remove('active'));
  btn.classList.add('active');

  // Mostrar/ocultar bloques
  document.querySelectorAll('.unit-block').forEach(block => {
    const match = unit === 'all' || parseInt(block.dataset.unit) === unit;
    block.style.display       = match ? '' : 'none';
    block.style.animation     = match ? 'fadeIn 0.3s ease' : '';
    block.style.opacity       = match ? '1' : '0';
  });

  // Colapsar tarjetas al cambiar de filtro
  document.querySelectorAll('.task-card.expanded')
    .forEach(c => c.classList.remove('expanded'));
}

/* ── Animación de entrada escalonada para cards ── */
function staggerCards() {
  const cards = document.querySelectorAll('.task-card');
  cards.forEach((card, i) => {
    card.style.animationDelay = (0.04 + i * 0.055) + 's';
    card.style.animationFillMode = 'both';
  });
}

/* ── Marcador de tarea activa en el sidebar de navegación ── */
function updateStats() {
  const cfg = PORTFOLIO_CONFIG;
  const els = {
    total:    document.querySelector('.stat-block:nth-child(1) .stat-num'),
    enviadas: document.querySelector('.stat-block:nth-child(2) .stat-num'),
    proceso:  document.querySelector('.stat-block:nth-child(3) .stat-num'),
    promedio: document.querySelector('.stat-block:nth-child(4) .stat-num'),
  };

  if (els.total)    els.total.textContent    = cfg.totalTareas;
  if (els.enviadas) els.enviadas.textContent = cfg.tareasEnviadas;
  if (els.proceso)  els.proceso.textContent  = cfg.tareasRevision + cfg.tareasPendientes - (cfg.totalTareas - cfg.tareasEnviadas - cfg.tareasRevision - 1);
  if (els.promedio) els.promedio.textContent = cfg.promedio;
}

/* ── Efecto hover dorado en número de tarea al hover ── */
function initCardHoverEffect() {
  document.querySelectorAll('.task-card').forEach(card => {
    const num = card.querySelector('.task-num');
    if (!num) return;
    card.addEventListener('mouseenter', () => {
      num.style.color = 'var(--gold)';
      num.style.transition = 'color 0.2s ease';
    });
    card.addEventListener('mouseleave', () => {
      if (!card.classList.contains('expanded')) {
        num.style.color = '';
      }
    });
  });
}

/* ── Cierre con tecla Escape ── */
document.addEventListener('keydown', e => {
  if (e.key === 'Escape') {
    document.querySelectorAll('.task-card.expanded')
      .forEach(c => {
        c.classList.remove('expanded');
        const num = c.querySelector('.task-num');
        if (num) num.style.color = '';
      });
  }
});

/* ── Init ── */
document.addEventListener('DOMContentLoaded', () => {
  animateProgress();
  staggerCards();
  initCardHoverEffect();
});
