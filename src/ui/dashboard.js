/**
 * CivicShield - Dashboard UI
 * Premium real-time infrastructure monitoring dashboard with glassmorphism design.
 * All components dynamically update with simulation data.
 */

let dashboardCallbacks = {};
let currentSession = null;

export function renderDashboard(container, session, callbacks) {
    currentSession = session;
    dashboardCallbacks = callbacks;
    const canInject = callbacks.hasPermission('Inject_Failure');
    const canRecover = callbacks.hasPermission('Trigger_Recovery');

    container.innerHTML = `
    <div class="dashboard-wrapper">
      <!-- TOP NAVIGATION BAR -->
      <nav class="topbar">
        <div class="topbar-left">
          <span class="topbar-logo">🛡️</span>
          <span class="topbar-title">CIVICSHIELD</span>
          <span class="topbar-divider"></span>
          <span class="topbar-subtitle">Infrastructure Stability Simulator</span>
        </div>
        <div class="topbar-center">
          <div class="sim-step-display">
            <span class="step-label">STEP</span>
            <span class="step-value" id="stepDisplay">0</span>
            <span class="step-total">/ 20</span>
          </div>
        </div>
        <div class="topbar-right">
          <div class="user-badge">
            <span class="user-icon">${session.icon}</span>
            <span class="user-name">${session.username}</span>
            <span class="user-role">${session.role}</span>
          </div>
          <button class="btn-logout" id="logoutBtn">⏻ Logout</button>
        </div>
      </nav>

      <!-- MAIN CONTENT -->
      <div class="dashboard-content">
        <!-- LEFT: SIMULATION CONTROLS + USS -->
        <aside class="sidebar-left">
          <!-- USS GAUGE -->
          <div class="uss-panel glass-panel">
            <div class="panel-header">
              <span class="panel-icon">📊</span>
              <span>Urban Stability Score</span>
            </div>
            <div class="uss-gauge-container">
              <svg class="uss-gauge" viewBox="0 0 200 200">
                <circle class="gauge-bg" cx="100" cy="100" r="85" />
                <circle class="gauge-fill" id="ussFill" cx="100" cy="100" r="85" />
                <text class="gauge-text" id="ussValue" x="100" y="95">100.0</text>
                <text class="gauge-label" id="ussThreat" x="100" y="120">LOW</text>
              </svg>
            </div>
            <div class="uss-threat-badge" id="threatBadge">
              <span class="threat-icon" id="threatIcon">🟢</span>
              <span class="threat-text" id="threatText">ALL SYSTEMS NOMINAL</span>
            </div>
            <div class="uss-weakest" id="weakestSector">
              Weakest: <strong>None</strong>
            </div>
          </div>

          <!-- SIMULATION CONTROLS -->
          <div class="controls-panel glass-panel">
            <div class="panel-header">
              <span class="panel-icon">🎮</span>
              <span>Simulation Controls</span>
            </div>
            <div class="controls-grid">
              <button class="ctrl-btn ctrl-start" id="startBtn">▶ Start</button>
              <button class="ctrl-btn ctrl-pause" id="pauseBtn" disabled>⏸ Pause</button>
              <button class="ctrl-btn ctrl-step" id="stepBtn">⏭ Step</button>
              <button class="ctrl-btn ctrl-reset" id="resetBtn">⟲ Reset</button>
            </div>
            <div class="speed-control">
              <label>Speed: <span id="speedValue">1.0</span>x</label>
              <input type="range" id="speedSlider" min="0.5" max="5" step="0.5" value="1" />
            </div>
            <div class="sim-status" id="simStatus">
              <span class="status-dot status-idle"></span>
              <span>Ready</span>
            </div>
          </div>

          <!-- MANUAL ACTIONS (role-gated) -->
          ${canInject || canRecover ? `
          <div class="actions-panel glass-panel">
            <div class="panel-header">
              <span class="panel-icon">⚡</span>
              <span>Manual Actions</span>
            </div>
            ${canInject ? `
            <div class="action-group">
              <span class="action-label">💥 Inject Failure</span>
              <div class="action-btns">
                <button class="action-btn action-danger" data-sub="Power" data-comp="1">⚡ Gen #1</button>
                <button class="action-btn action-danger" data-sub="Power" data-comp="3">⚡ Gen #3</button>
                <button class="action-btn action-danger" data-sub="Water" data-comp="1">💧 Pump #1</button>
                <button class="action-btn action-danger" data-sub="Transport" data-comp="3">🚥 Int #3</button>
              </div>
            </div>` : ''}
            ${canRecover ? `
            <div class="action-group">
              <span class="action-label">🔧 Recovery</span>
              <button class="action-btn action-success" id="recoveryBtn">🔄 Trigger Recovery</button>
            </div>` : ''}
          </div>` : ''}
        </aside>

        <!-- CENTER: SUBSYSTEM PANELS -->
        <main class="main-area">
          <!-- SUBSYSTEM CARDS -->
          <div class="subsystem-grid">
            <div class="subsystem-card glass-panel" id="panel-power">
              <div class="sub-header">
                <span class="sub-icon sub-icon-power">⚡</span>
                <span class="sub-name">Power Grid</span>
                <span class="sub-status-badge" id="badge-power">Normal</span>
              </div>
              <div class="sub-health-bar">
                <div class="health-fill" id="health-power" style="width:100%"></div>
              </div>
              <div class="sub-health-value"><span id="healthVal-power">100.0</span>%</div>
              <div class="sub-details" id="details-power">
                <div class="detail-row"><span>Generators</span><span id="det-power-gen">5/5 online</span></div>
                <div class="detail-row"><span>Capacity</span><span id="det-power-cap">1000 MW</span></div>
                <div class="detail-row"><span>Load</span><span id="det-power-load">0 MW</span></div>
              </div>
              <div class="sub-components" id="comp-power"></div>
            </div>

            <div class="subsystem-card glass-panel" id="panel-water">
              <div class="sub-header">
                <span class="sub-icon sub-icon-water">💧</span>
                <span class="sub-name">Water Network</span>
                <span class="sub-status-badge" id="badge-water">Normal</span>
              </div>
              <div class="sub-health-bar">
                <div class="health-fill" id="health-water" style="width:100%"></div>
              </div>
              <div class="sub-health-value"><span id="healthVal-water">100.0</span>%</div>
              <div class="sub-details" id="details-water">
                <div class="detail-row"><span>Pumps</span><span id="det-water-pumps">6/6 active</span></div>
                <div class="detail-row"><span>Reservoirs</span><span id="det-water-res">3 operational</span></div>
                <div class="detail-row"><span>Avg Level</span><span id="det-water-level">0%</span></div>
              </div>
              <div class="sub-components" id="comp-water"></div>
            </div>

            <div class="subsystem-card glass-panel" id="panel-transport">
              <div class="sub-header">
                <span class="sub-icon sub-icon-transport">🚥</span>
                <span class="sub-name">Transport Control</span>
                <span class="sub-status-badge" id="badge-transport">Normal</span>
              </div>
              <div class="sub-health-bar">
                <div class="health-fill" id="health-transport" style="width:100%"></div>
              </div>
              <div class="sub-health-value"><span id="healthVal-transport">100.0</span>%</div>
              <div class="sub-details" id="details-transport">
                <div class="detail-row"><span>Intersections</span><span id="det-transport-ix">12/12 active</span></div>
                <div class="detail-row"><span>Avg Congestion</span><span id="det-transport-cong">0%</span></div>
              </div>
              <div class="sub-components" id="comp-transport"></div>
            </div>

            <div class="subsystem-card glass-panel" id="panel-emergency">
              <div class="sub-header">
                <span class="sub-icon sub-icon-emergency">🚨</span>
                <span class="sub-name">Emergency Response</span>
                <span class="sub-status-badge" id="badge-emergency">Normal</span>
              </div>
              <div class="sub-health-bar">
                <div class="health-fill" id="health-emergency" style="width:100%"></div>
              </div>
              <div class="sub-health-value"><span id="healthVal-emergency">100.0</span>%</div>
              <div class="sub-details" id="details-emergency">
                <div class="detail-row"><span>Units</span><span id="det-em-units">15/15 operational</span></div>
                <div class="detail-row"><span>Available</span><span id="det-em-avail">15</span></div>
                <div class="detail-row"><span>Deployed</span><span id="det-em-deployed">0</span></div>
              </div>
              <div class="sub-components" id="comp-emergency"></div>
            </div>

            <div class="subsystem-card glass-panel" id="panel-healthcare">
              <div class="sub-header">
                <span class="sub-icon sub-icon-healthcare">🏥</span>
                <span class="sub-name">Healthcare</span>
                <span class="sub-status-badge" id="badge-healthcare">Normal</span>
              </div>
              <div class="sub-health-bar">
                <div class="health-fill" id="health-healthcare" style="width:100%"></div>
              </div>
              <div class="sub-health-value"><span id="healthVal-healthcare">100.0</span>%</div>
              <div class="sub-details" id="details-healthcare">
                <div class="detail-row"><span>Beds</span><span id="det-hc-beds">0/750 occupied</span></div>
                <div class="detail-row"><span>ICU</span><span id="det-hc-icu">0/105 occupied</span></div>
                <div class="detail-row"><span>Surge</span><span id="det-hc-surge">Inactive</span></div>
              </div>
              <div class="sub-components" id="comp-healthcare"></div>
            </div>
          </div>

          <!-- USS HISTORY CHART -->
          <div class="chart-panel glass-panel">
            <div class="panel-header">
              <span class="panel-icon">📈</span>
              <span>USS History</span>
            </div>
            <canvas id="ussChart" width="800" height="200"></canvas>
          </div>
        </main>

        <!-- RIGHT: EVENT LOG -->
        <aside class="sidebar-right">
          <div class="log-panel glass-panel">
            <div class="panel-header">
              <span class="panel-icon">📝</span>
              <span>Event Log</span>
              <span class="log-count" id="logCount">0</span>
            </div>
            <div class="log-filters">
              <button class="log-filter active" data-filter="all">All</button>
              <button class="log-filter" data-filter="ERROR">❌ Error</button>
              <button class="log-filter" data-filter="WARNING">⚠️ Warn</button>
              <button class="log-filter" data-filter="INFO">ℹ️ Info</button>
            </div>
            <div class="log-entries" id="logEntries"></div>
          </div>

          <!-- CASCADE PANEL -->
          <div class="cascade-panel glass-panel">
            <div class="panel-header">
              <span class="panel-icon">💥</span>
              <span>Cascade Status</span>
            </div>
            <div class="cascade-info" id="cascadeInfo">
              <div class="cascade-stat">
                <span class="cascade-label">Active Cascades</span>
                <span class="cascade-value" id="cascadeCount">0</span>
              </div>
              <div class="cascade-stat">
                <span class="cascade-label">Recovery Actions</span>
                <span class="cascade-value" id="recoveryCount">0</span>
              </div>
              <div class="cascade-deps" id="cascadeDeps"></div>
            </div>
          </div>
        </aside>
      </div>

      <!-- FINAL REPORT MODAL -->
      <div class="modal-overlay" id="reportModal" style="display:none;">
        <div class="modal-content glass-panel">
          <div class="modal-header">
            <h2>📊 Final Stability Report</h2>
            <button class="modal-close" id="closeModal">✕</button>
          </div>
          <div class="modal-body" id="reportBody"></div>
        </div>
      </div>
    </div>
  `;

    bindEvents(callbacks);
}

function bindEvents(callbacks) {
    document.getElementById('logoutBtn').addEventListener('click', callbacks.onLogout);

    document.getElementById('startBtn').addEventListener('click', () => {
        const state = callbacks.getState();
        if (state.isPaused) {
            callbacks.onResume();
        } else {
            callbacks.onStart();
        }
        updateControlState();
    });

    document.getElementById('pauseBtn').addEventListener('click', () => {
        callbacks.onPause();
        updateControlState();
    });

    document.getElementById('stepBtn').addEventListener('click', () => {
        callbacks.onStep();
    });

    document.getElementById('resetBtn').addEventListener('click', () => {
        callbacks.onReset();
        ussHistoryData = [];
        updateControlState();
    });

    document.getElementById('speedSlider').addEventListener('input', (e) => {
        const speed = parseFloat(e.target.value);
        callbacks.onSpeedChange(speed);
        document.getElementById('speedValue').textContent = speed.toFixed(1);
    });

    document.querySelectorAll('.action-btn.action-danger').forEach(btn => {
        btn.addEventListener('click', () => {
            callbacks.onInjectFailure(btn.dataset.sub, btn.dataset.comp);
            btn.classList.add('btn-flash');
            setTimeout(() => btn.classList.remove('btn-flash'), 300);
        });
    });

    const recoveryBtn = document.getElementById('recoveryBtn');
    if (recoveryBtn) {
        recoveryBtn.addEventListener('click', () => {
            callbacks.onTriggerRecovery();
        });
    }

    document.querySelectorAll('.log-filter').forEach(btn => {
        btn.addEventListener('click', () => {
            document.querySelectorAll('.log-filter').forEach(b => b.classList.remove('active'));
            btn.classList.add('active');
            currentLogFilter = btn.dataset.filter;
            if (lastSnapshot) renderLogEntries(lastSnapshot.logs);
        });
    });

    const closeModal = document.getElementById('closeModal');
    if (closeModal) {
        closeModal.addEventListener('click', () => {
            document.getElementById('reportModal').style.display = 'none';
        });
    }
}

let currentLogFilter = 'all';
let lastSnapshot = null;
let ussHistoryData = [];

function updateControlState() {
    setTimeout(() => {
        const state = dashboardCallbacks.getState();
        const startBtn = document.getElementById('startBtn');
        const pauseBtn = document.getElementById('pauseBtn');
        const simStatus = document.getElementById('simStatus');

        if (state.isRunning && !state.isPaused) {
            startBtn.textContent = '▶ Running';
            startBtn.disabled = true;
            pauseBtn.disabled = false;
            pauseBtn.textContent = '⏸ Pause';
            simStatus.innerHTML = '<span class="status-dot status-running"></span><span>Running (Step ' + state.currentStep + '/' + state.maxSteps + ')</span>';
        } else if (state.isPaused) {
            startBtn.textContent = '▶ Resume';
            startBtn.disabled = false;
            pauseBtn.disabled = true;
            simStatus.innerHTML = '<span class="status-dot status-paused"></span><span>Paused (Step ' + state.currentStep + '/' + state.maxSteps + ')</span>';
        } else {
            startBtn.textContent = '▶ Start';
            startBtn.disabled = false;
            pauseBtn.disabled = true;
            if (state.currentStep >= state.maxSteps) {
                simStatus.innerHTML = '<span class="status-dot status-complete"></span><span>Complete</span>';
            } else {
                simStatus.innerHTML = '<span class="status-dot status-idle"></span><span>Ready</span>';
            }
        }
    }, 50);
}

export function updateDashboard(snapshot, session) {
    lastSnapshot = snapshot;

    document.getElementById('stepDisplay').textContent = snapshot.step || 0;

    updateSubsystem('power', snapshot.power);
    updateSubsystem('water', snapshot.water);
    updateSubsystem('transport', snapshot.transport);
    updateSubsystem('emergency', snapshot.emergency);
    updateSubsystem('healthcare', snapshot.healthcare);

    updateUSS(snapshot.stability);
    updateCascade(snapshot.cascade);
    renderLogEntries(snapshot.logs);
    updateControlState();

    if (snapshot.stability) {
        ussHistoryData.push({ step: snapshot.step, uss: snapshot.stability.uss, threat: snapshot.stability.threat });
        drawUSSChart();
    }

    updateDetailedInfo(snapshot);
}

function updateSubsystem(key, data) {
    if (!data) return;

    const badge = document.getElementById(`badge-${key}`);
    const healthFill = document.getElementById(`health-${key}`);
    const healthVal = document.getElementById(`healthVal-${key}`);
    const panel = document.getElementById(`panel-${key}`);

    if (badge) {
        badge.textContent = data.status;
        badge.className = 'sub-status-badge status-' + data.status.toLowerCase();
    }

    if (healthFill) {
        healthFill.style.width = data.health.toFixed(1) + '%';
        healthFill.className = 'health-fill health-' + getHealthColor(data.health);
    }

    if (healthVal) {
        healthVal.textContent = data.health.toFixed(1);
    }

    if (panel) {
        panel.className = 'subsystem-card glass-panel panel-' + data.status.toLowerCase();
    }
}

function updateDetailedInfo(snapshot) {
    if (snapshot.power) {
        const p = snapshot.power;
        safeSet('det-power-gen', `${p.onlineCount}/${p.totalCount} online`);
        safeSet('det-power-cap', `${p.onlineCount * 200} MW`);
        safeSet('det-power-load', `${p.currentLoad.toFixed(0)} MW`);
        renderComponents('comp-power', p.generators.map(g => ({
            label: `Gen #${g.id}`,
            online: g.isOnline,
            value: g.isOnline ? `${g.currentLoad.toFixed(0)} MW` : 'OFFLINE'
        })));
    }

    if (snapshot.water) {
        const w = snapshot.water;
        safeSet('det-water-pumps', `${w.activePumps}/${w.totalPumps} active`);
        safeSet('det-water-res', `3 operational`);
        safeSet('det-water-level', `${w.avgReservoirLevel.toFixed(1)}%`);
        renderComponents('comp-water', w.pumps.map(p => ({
            label: `Pump #${p.id}`,
            online: p.isOperational,
            value: p.isOperational ? `${p.flowRate.toFixed(0)} L/min` : 'FAILED'
        })));
    }

    if (snapshot.transport) {
        const t = snapshot.transport;
        safeSet('det-transport-ix', `${t.operationalCount}/${t.totalCount} active`);
        safeSet('det-transport-cong', `${t.avgCongestion.toFixed(1)}%`);
        renderComponents('comp-transport', t.intersections.slice(0, 6).map(i => ({
            label: `Int #${i.id}`,
            online: i.isOperational && !i.incidentActive,
            value: i.incidentActive ? '⚠️' : getPhaseIcon(i.phase)
        })));
    }

    if (snapshot.emergency) {
        const e = snapshot.emergency;
        safeSet('det-em-units', `${e.operationalCount}/${e.totalCount} operational`);
        safeSet('det-em-avail', e.availableCount.toString());
        safeSet('det-em-deployed', e.deployedCount.toString());
        renderComponents('comp-emergency', e.byType.map(t => ({
            label: `${t.icon} ${t.type}`,
            online: t.operational > 0,
            value: `${t.available}/${t.total}`
        })));
    }

    if (snapshot.healthcare) {
        const h = snapshot.healthcare;
        safeSet('det-hc-beds', `${h.occupiedBeds}/${h.totalBeds} occupied`);
        safeSet('det-hc-icu', `${h.occupiedICU}/${h.totalICU} occupied`);
        safeSet('det-hc-surge', h.surgeActive ? '🔴 Active' : '🟢 Inactive');
        renderComponents('comp-healthcare', h.hospitals.map(hosp => ({
            label: `H${hosp.id}`,
            online: hosp.isOperational,
            value: `${hosp.occupiedBeds}/${hosp.beds}`
        })));
    }
}

function renderComponents(containerId, items) {
    const el = document.getElementById(containerId);
    if (!el) return;
    el.innerHTML = items.map(item => `
    <span class="comp-chip ${item.online ? 'comp-online' : 'comp-offline'}">
      <span class="comp-label">${item.label}</span>
      <span class="comp-value">${item.value}</span>
    </span>
  `).join('');
}

function updateUSS(stability) {
    if (!stability) return;

    const ussValue = document.getElementById('ussValue');
    const ussThreat = document.getElementById('ussThreat');
    const ussFill = document.getElementById('ussFill');
    const threatBadge = document.getElementById('threatBadge');
    const threatIcon = document.getElementById('threatIcon');
    const threatText = document.getElementById('threatText');
    const weakest = document.getElementById('weakestSector');

    if (ussValue) ussValue.textContent = stability.uss.toFixed(1);
    if (ussThreat) ussThreat.textContent = stability.threat;

    if (ussFill) {
        const circumference = 2 * Math.PI * 85;
        const offset = circumference - (stability.uss / 100) * circumference;
        ussFill.style.strokeDasharray = circumference;
        ussFill.style.strokeDashoffset = offset;
        ussFill.style.stroke = getThreatColor(stability.threat);
    }

    const threatMessages = {
        LOW: 'ALL SYSTEMS NOMINAL',
        MEDIUM: 'DEGRADED PERFORMANCE',
        HIGH: 'MULTIPLE FAILURES DETECTED',
        CRITICAL: 'SYSTEMIC COLLAPSE IMMINENT'
    };

    const threatIcons = { LOW: '🟢', MEDIUM: '🟡', HIGH: '🟠', CRITICAL: '🔴' };

    if (threatBadge) threatBadge.className = 'uss-threat-badge threat-' + stability.threat.toLowerCase();
    if (threatIcon) threatIcon.textContent = threatIcons[stability.threat] || '⚪';
    if (threatText) threatText.textContent = threatMessages[stability.threat] || '';
    if (weakest) weakest.innerHTML = `Weakest: <strong>${stability.weakestSector}</strong>`;
}

function updateCascade(cascade) {
    if (!cascade) return;
    safeSet('cascadeCount', cascade.totalCascades.toString());
    safeSet('recoveryCount', cascade.recoveryActions.toString());
}

function renderLogEntries(logs) {
    if (!logs) return;
    const container = document.getElementById('logEntries');
    const countEl = document.getElementById('logCount');
    if (!container) return;

    let filtered = logs;
    if (currentLogFilter !== 'all') {
        filtered = logs.filter(l => l.severity === currentLogFilter);
    }

    const entries = filtered.slice(-30).reverse();
    container.innerHTML = entries.map(entry => `
    <div class="log-entry log-${entry.severity.toLowerCase()}">
      <span class="log-step">[${entry.step}]</span>
      <span class="log-cat">${entry.categoryIcon} ${entry.category}</span>
      <span class="log-sev">${entry.severityIcon}</span>
      <span class="log-msg">${entry.message}</span>
    </div>
  `).join('');

    if (countEl) countEl.textContent = logs.length;
    container.scrollTop = 0;
}

function drawUSSChart() {
    const canvas = document.getElementById('ussChart');
    if (!canvas || ussHistoryData.length < 2) return;

    const ctx = canvas.getContext('2d');
    const w = canvas.width;
    const h = canvas.height;
    const padding = 40;

    ctx.clearRect(0, 0, w, h);

    ctx.strokeStyle = 'rgba(255,255,255,0.1)';
    ctx.lineWidth = 1;
    for (let y = 0; y <= 100; y += 20) {
        const py = h - padding - ((y / 100) * (h - padding * 2));
        ctx.beginPath();
        ctx.moveTo(padding, py);
        ctx.lineTo(w - padding, py);
        ctx.stroke();

        ctx.fillStyle = 'rgba(255,255,255,0.4)';
        ctx.font = '10px JetBrains Mono';
        ctx.textAlign = 'right';
        ctx.fillText(y.toString(), padding - 5, py + 3);
    }

    ctx.fillStyle = 'rgba(239,68,68,0.08)';
    ctx.fillRect(padding, h - padding - ((30 / 100) * (h - padding * 2)), w - padding * 2, (30 / 100) * (h - padding * 2));

    ctx.fillStyle = 'rgba(249,115,22,0.06)';
    const y30 = h - padding - ((30 / 100) * (h - padding * 2));
    const y60 = h - padding - ((60 / 100) * (h - padding * 2));
    ctx.fillRect(padding, y60, w - padding * 2, y30 - y60);

    const dataW = w - padding * 2;
    const dataH = h - padding * 2;
    const stepW = dataW / Math.max(1, 20);

    const gradient = ctx.createLinearGradient(0, padding, 0, h - padding);
    gradient.addColorStop(0, 'rgba(34,197,94,0.3)');
    gradient.addColorStop(0.5, 'rgba(234,179,8,0.2)');
    gradient.addColorStop(1, 'rgba(239,68,68,0.1)');

    ctx.beginPath();
    ctx.moveTo(padding, h - padding);
    for (let i = 0; i < ussHistoryData.length; i++) {
        const x = padding + i * stepW;
        const y = h - padding - (ussHistoryData[i].uss / 100) * dataH;
        ctx.lineTo(x, y);
    }
    ctx.lineTo(padding + (ussHistoryData.length - 1) * stepW, h - padding);
    ctx.closePath();
    ctx.fillStyle = gradient;
    ctx.fill();

    ctx.beginPath();
    for (let i = 0; i < ussHistoryData.length; i++) {
        const x = padding + i * stepW;
        const y = h - padding - (ussHistoryData[i].uss / 100) * dataH;
        if (i === 0) ctx.moveTo(x, y);
        else ctx.lineTo(x, y);
    }
    ctx.strokeStyle = getThreatColor(ussHistoryData[ussHistoryData.length - 1].threat);
    ctx.lineWidth = 2.5;
    ctx.stroke();

    for (let i = 0; i < ussHistoryData.length; i++) {
        const x = padding + i * stepW;
        const y = h - padding - (ussHistoryData[i].uss / 100) * dataH;
        ctx.beginPath();
        ctx.arc(x, y, 3, 0, Math.PI * 2);
        ctx.fillStyle = getThreatColor(ussHistoryData[i].threat);
        ctx.fill();
    }
}

export function showFinalReport(report) {
    const modal = document.getElementById('reportModal');
    const body = document.getElementById('reportBody');
    if (!modal || !body) return;

    const threatColor = getThreatColor(report.finalThreat);

    body.innerHTML = `
    <div class="report-section">
      <div class="report-uss">
        <div class="report-uss-value" style="color: ${threatColor}">${report.finalUSS.toFixed(2)}</div>
        <div class="report-uss-label">Final Urban Stability Score</div>
        <div class="report-threat" style="background: ${threatColor}20; color: ${threatColor}; border: 1px solid ${threatColor}">${report.finalThreat} THREAT</div>
      </div>
    </div>

    <div class="report-section">
      <h3>📊 Subsystem Health Summary</h3>
      <div class="report-grid">
        ${Object.entries(report.analysis.healthBreakdown || {}).map(([key, val]) => `
          <div class="report-stat">
            <span class="report-stat-label">${getSubIcon(key)} ${key}</span>
            <div class="report-stat-bar"><div class="report-stat-fill" style="width:${val}%; background: ${getHealthBarColor(val)}"></div></div>
            <span class="report-stat-value">${val.toFixed(1)}%</span>
          </div>
        `).join('')}
      </div>
    </div>

    <div class="report-section">
      <h3>💥 Cascade Events: ${report.cascadeHistory.length}</h3>
      <div class="report-cascades">
        ${report.cascadeHistory.slice(-10).map(c => `
          <div class="report-cascade-item">
            ${getSubIcon(c.source)} ${c.source} → ${getSubIcon(c.target)} ${c.target}: -${c.degradation}%
          </div>
        `).join('')}
      </div>
    </div>

    <div class="report-section">
      <h3>🏆 Simulation Summary</h3>
      <p>The CivicShield simulation completed <strong>${report.totalSteps}</strong> steps with a final USS of <strong>${report.finalUSS.toFixed(2)}</strong>.
      The weakest sector was <strong>${report.analysis.weakestSector}</strong>.
      A total of <strong>${report.cascadeHistory.length}</strong> cascade events were detected during the simulation.</p>
    </div>
  `;

    modal.style.display = 'flex';
}

function getHealthColor(health) {
    if (health >= 80) return 'green';
    if (health >= 50) return 'yellow';
    if (health >= 20) return 'orange';
    return 'red';
}

function getThreatColor(threat) {
    switch (threat) {
        case 'LOW': return '#22c55e';
        case 'MEDIUM': return '#eab308';
        case 'HIGH': return '#f97316';
        case 'CRITICAL': return '#ef4444';
        default: return '#6b7280';
    }
}

function getHealthBarColor(val) {
    if (val >= 80) return '#22c55e';
    if (val >= 50) return '#eab308';
    if (val >= 20) return '#f97316';
    return '#ef4444';
}

function getPhaseIcon(phase) {
    switch (phase) {
        case 'Green': return '🟢';
        case 'Yellow': return '🟡';
        case 'Red': return '🔴';
        case 'Flashing': return '⚠️';
        default: return '⚪';
    }
}

function getSubIcon(name) {
    const icons = { Power: '⚡', Water: '💧', Transport: '🚥', Emergency: '🚨', Healthcare: '🏥' };
    return icons[name] || '📋';
}

function safeSet(id, text) {
    const el = document.getElementById(id);
    if (el) el.textContent = text;
}
