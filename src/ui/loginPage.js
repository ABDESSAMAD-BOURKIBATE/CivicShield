/**
 * CivicShield - Login Page UI
 * Premium glassmorphism login with role-based credential hints.
 */

import * as AccessControl from '../engine/accessControl.js';

export function renderLogin(container, onSuccess) {
    container.innerHTML = `
    <div class="login-wrapper">
      <div class="login-particles" id="particles"></div>
      <div class="login-container">
        <div class="login-card">
          <div class="login-header">
            <div class="login-shield">🛡️</div>
            <h1 class="login-title">CIVICSHIELD</h1>
            <p class="login-subtitle">Public Infrastructure Stability Simulator</p>
            <div class="login-version">v2.0 | High-Integrity Engine</div>
          </div>

          <form id="loginForm" class="login-form" autocomplete="off">
            <div class="form-group">
              <label for="username">
                <span class="label-icon">👤</span>
                Username
              </label>
              <input type="text" id="username" name="username" placeholder="Enter username" required autocomplete="off" />
            </div>

            <div class="form-group">
              <label for="password">
                <span class="label-icon">🔒</span>
                Password
              </label>
              <input type="password" id="password" name="password" placeholder="Enter password" required autocomplete="off" />
            </div>

            <div id="loginError" class="login-error" style="display:none;"></div>

            <button type="submit" id="loginBtn" class="login-btn">
              <span class="btn-text">AUTHENTICATE</span>
              <span class="btn-icon">→</span>
            </button>
          </form>

          <div class="credential-hints">
            <div class="hint-title">🔑 Simulation Credentials</div>
            <div class="hint-grid">
              <div class="hint-card" data-user="admin" data-pass="shield2026">
                <span class="hint-icon">👑</span>
                <span class="hint-role">Admin</span>
                <span class="hint-perm">Full Access</span>
              </div>
              <div class="hint-card" data-user="operator" data-pass="ops2026">
                <span class="hint-icon">🔧</span>
                <span class="hint-role">Operator</span>
                <span class="hint-perm">Operations</span>
              </div>
              <div class="hint-card" data-user="observer" data-pass="view2026">
                <span class="hint-icon">👁️</span>
                <span class="hint-role">Observer</span>
                <span class="hint-perm">Read-only</span>
              </div>
            </div>
          </div>
        </div>

        <div class="login-footer">
          <span>🏙️ Urban Infrastructure Simulation Engine</span>
          <span>© 2026 ABDESSAMAD BOURKIBATE</span>
        </div>
      </div>
    </div>
  `;

    createParticles();

    const form = document.getElementById('loginForm');
    const errorDiv = document.getElementById('loginError');

    form.addEventListener('submit', (e) => {
        e.preventDefault();
        const username = document.getElementById('username').value.trim();
        const password = document.getElementById('password').value.trim();

        const result = AccessControl.authenticate(username, password);

        if (result.success) {
            const card = document.querySelector('.login-card');
            card.classList.add('login-success');
            setTimeout(() => onSuccess(result.session), 600);
        } else {
            errorDiv.textContent = '❌ ' + result.error;
            errorDiv.style.display = 'block';
            form.classList.add('shake');
            setTimeout(() => form.classList.remove('shake'), 500);
        }
    });

    document.querySelectorAll('.hint-card').forEach(card => {
        card.addEventListener('click', () => {
            document.getElementById('username').value = card.dataset.user;
            document.getElementById('password').value = card.dataset.pass;
            document.querySelectorAll('.hint-card').forEach(c => c.classList.remove('hint-active'));
            card.classList.add('hint-active');
        });
    });
}

function createParticles() {
    const container = document.getElementById('particles');
    if (!container) return;
    for (let i = 0; i < 50; i++) {
        const particle = document.createElement('div');
        particle.className = 'particle';
        particle.style.left = Math.random() * 100 + '%';
        particle.style.top = Math.random() * 100 + '%';
        particle.style.animationDelay = Math.random() * 8 + 's';
        particle.style.animationDuration = (4 + Math.random() * 6) + 's';
        particle.style.width = particle.style.height = (2 + Math.random() * 4) + 'px';
        container.appendChild(particle);
    }
}
