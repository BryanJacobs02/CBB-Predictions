import subprocess, sys, os

venv_path = os.path.join(os.path.dirname(__file__), "..", ".venv")

# Create venv using the system Python
subprocess.run([sys.executable, "-m", "venv", venv_path], check=True)

pip = os.path.join(venv_path, "bin", "pip")

# Upgrade pip first
subprocess.run([pip, "install", "--upgrade", "pip"], check=True)

# Install torch first, alone, before anything else
print("Installing torch...")
subprocess.run([
    pip, "install",
    "torch==2.2.0",
    "--index-url", "https://download.pytorch.org/whl/cpu"
], check=True)

# Then install the rest
print("Installing remaining packages...")
remaining = [
    "torch-geometric==2.5.0",
    "numpy==1.26.4",
    "pandas==2.2.0",
    "scikit-learn==1.4.0",
]
subprocess.run([pip, "install"] + remaining, check=True)

print("Environment ready.")
