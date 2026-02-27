# start_environment.ps1
# Automates the downloading and setup of GNAT (Ada) and Maven (Java), compiles both, and runs them together.

Write-Host "====================================================="
Write-Host "   CIVICSHIELD: COMPLETE ENVIRONMENT INITIALIZATION  "
Write-Host "====================================================="

$InitialLocation = Get-Location

# 1. Setup Alire (Ada Package Manager)
Write-Host "1. Installing Alire (Ada Compiler Toolchain)..."
if (-not (Test-Path "alr.exe")) {
    Invoke-WebRequest -Uri "https://github.com/alire-project/alire/releases/download/v2.0.1/alr-2.0.1-bin-x86_64-windows.zip" -OutFile "alr.zip"
    Expand-Archive -Path "alr.zip" -DestinationPath "alr_ext" -Force
    Move-Item -Path "alr_ext\*\alr.exe" -Destination ".\alr.exe" -Force
    Remove-Item "alr_ext" -Recurse -Force
}

# 2. Setup GNAT via Alire
Write-Host "2. Configuring GNAT and Building Ada Engine..."
# Download and select the toolchain automatically
.\alr.exe --non-interactive toolchain --select gnat_native --disable-assistant
.\alr.exe --non-interactive toolchain --select gprbuild --disable-assistant

# Alire creates a local environment, we can use alr exec to run gprbuild
.\alr.exe exec -- gprbuild -P civicshield.gpr

# 3. Setup Maven
Write-Host "3. Installing Apache Maven (Java Build Tool)..."
Set-Location "$InitialLocation\desktop-ui"
if (-not (Test-Path "apache-maven-3.9.6\bin\mvn.cmd")) {
    Invoke-WebRequest -Uri "https://archive.apache.org/dist/maven/maven-3/3.9.6/binaries/apache-maven-3.9.6-bin.zip" -OutFile "maven.zip"
    Expand-Archive -Path "maven.zip" -DestinationPath "." -Force
}

# 4. Start Ada Engine
Write-Host "4. Starting Ada Physics Engine (TCP Port 9100)..."
Set-Location $InitialLocation
Start-Process -FilePath ".\bin\civicshield.exe" -WindowStyle Minimized

# 5. Start Java UI
Write-Host "5. Compiling and Starting Java SCADA Dashboard..."
Set-Location "$InitialLocation\desktop-ui"
.\apache-maven-3.9.6\bin\mvn.cmd clean javafx:run

Write-Host "====================================================="
Write-Host "   SYSTEM ONLINE. YOU MAY NOW CLOSE THIS TERMINAL.   "
Write-Host "====================================================="
