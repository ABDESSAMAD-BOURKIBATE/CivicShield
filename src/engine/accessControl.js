/**
 * CivicShield - Access Control Module (RBAC)
 * Role-Based Access Control with three privilege tiers:
 * Administrator, Operator, Observer
 */

const USERS = [
  { username: 'admin', password: 'shield2026', role: 'Administrator', icon: '👑' },
  { username: 'operator', password: 'ops2026', role: 'Operator', icon: '🔧' },
  { username: 'observer', password: 'view2026', role: 'Observer', icon: '👁️' }
];

const PERMISSIONS = {
  Administrator: ['View_Dashboard', 'View_Logs', 'Inject_Failure', 'Trigger_Recovery', 'Modify_Config', 'Manage_Users'],
  Operator: ['View_Dashboard', 'View_Logs', 'Inject_Failure', 'Trigger_Recovery'],
  Observer: ['View_Dashboard', 'View_Logs']
};

const PERMISSION_LABELS = {
  View_Dashboard: { label: 'View Dashboard', icon: '📺' },
  View_Logs: { label: 'View Logs', icon: '📝' },
  Inject_Failure: { label: 'Inject Failure', icon: '💥' },
  Trigger_Recovery: { label: 'Trigger Recovery', icon: '🔄' },
  Modify_Config: { label: 'Modify Config', icon: '⚙️' },
  Manage_Users: { label: 'Manage Users', icon: '👥' }
};

let currentSession = null;
let accessLog = [];

export function initialize() {
  currentSession = null;
  accessLog = [];
}

export function authenticate(username, password) {
  const user = USERS.find(u => u.username === username && u.password === password);

  if (user) {
    currentSession = {
      username: user.username,
      role: user.role,
      icon: user.icon,
      permissions: PERMISSIONS[user.role],
      loginTime: new Date().toISOString(),
      token: generateToken()
    };

    logAccess(currentSession.username, 'LOGIN', true, 'User authenticated successfully');
    return { success: true, session: { ...currentSession } };
  }

  logAccess(username, 'LOGIN', false, 'Invalid credentials');
  return { success: false, error: 'Invalid username or password' };
}

export function logout() {
  if (currentSession) {
    logAccess(currentSession.username, 'LOGOUT', true, 'User logged out');
    currentSession = null;
  }
}

export function hasPermission(action) {
  if (!currentSession) return false;
  return currentSession.permissions.includes(action);
}

export function getCurrentSession() {
  return currentSession ? { ...currentSession } : null;
}

export function getAccessLog() {
  return [...accessLog];
}

export function getAllPermissions() {
  return { ...PERMISSION_LABELS };
}

export function getRolePermissions(role) {
  return PERMISSIONS[role] || [];
}

function logAccess(username, action, success, details) {
  accessLog.push({
    timestamp: new Date().toISOString(),
    username,
    action,
    success,
    details,
    id: accessLog.length + 1
  });
}

function generateToken() {
  return 'CS-' + Date.now().toString(36) + '-' + Math.random().toString(36).substring(2, 10);
}

export function getAvailableRoles() {
  return USERS.map(u => ({
    username: u.username,
    role: u.role,
    icon: u.icon,
    permissions: PERMISSIONS[u.role].map(p => PERMISSION_LABELS[p])
  }));
}
