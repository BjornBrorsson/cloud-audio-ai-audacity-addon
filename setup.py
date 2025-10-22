"""Setup script for Audacity Cloud AI Plugin."""

from setuptools import setup, find_packages
from pathlib import Path

# Read README for long description
readme_file = Path(__file__).parent / 'README.md'
long_description = readme_file.read_text(encoding='utf-8') if readme_file.exists() else ''

setup(
    name='audacity-cloudai',
    version='1.0.0',
    description='Audacity plugin for ElevenLabs and potentially other Cloud AI Audio services',
    long_description=long_description,
    long_description_content_type='text/markdown',
    author='Björn Brorsson',
    author_email='bjorn.fristrom@gmail.com',
    url='https://github.com/BjornBrorsson/cloud-audio-ai-audacity-addon',
    license='MIT',
    packages=find_packages(where='src'),
    package_dir={'': 'src'},
    python_requires='>=3.8',
    install_requires=[
        "requests>=2.31.0",
        "pydub>=0.25.1",
        "numpy>=1.24.0",
        "python-dotenv>=1.0.0",
    ],
    extras_require={
        "dev": [
            "pytest>=7.4.0",
            "pytest-cov>=4.1.0",
            "pylint>=2.17.0",
            "black>=23.7.0",
            "isort>=5.12.0",
            "flake8>=6.0.0",
        ],
    },
    entry_points={
        "console_scripts": [
            "audacity-cloudai=audacity_cloudai:main",
            "audacity-cloudai-gui=gui_launcher:main",
        ],
    },
    include_package_data=True,
    package_data={
        "": ["*.md", "*.txt", "*.yml", "*.yaml"],
    },
    zip_safe=False,
)
