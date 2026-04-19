# 🚀 OdontoGest – Deployment Version

This branch contains a **production-ready version** of the OdontoGest platform, optimized for cloud deployment on **Heroku using Docker**.

> ⚠️ This is a **lightweight build**. Development resources, raw datasets, and training scripts have been removed to ensure faster build times and cleaner deployment.
## 🚧 Deployment Architecture

The OdontoGest platform follows a **containerized cloud deployment strategy**, designed to ensure portability, scalability, and reproducibility across environments.

---

### 🐳 Containerization (Docker)

The application is packaged using **Docker**, encapsulating:

- R runtime environment
- Shiny Server
- System-level dependencies (e.g., Java, Chromium for PDF generation)
- Required R packages

#### 🔧 Base Image
- `rocker/shiny-verse`  
  Provides a preconfigured environment with:
  - R + Shiny Server
  - Tidyverse ecosystem
  - Preinstalled system libraries

#### ⚙️ Key Features

- **Environment consistency** → Same behavior in development and production  
- **Dependency isolation** → No conflicts with host system  
- **Dynamic port binding** → Compatible with cloud providers (e.g., Heroku assigns ports dynamically)

---

### ☁️ Cloud Deployment (Heroku)

The application is deployed on **Heroku (PaaS)**, which abstracts infrastructure management.

#### 🧱 Architecture Overview

- **Web container (Docker)** → Runs the Shiny application  
- **External database (JawsDB MySQL)** → Persistent data storage  
- **Buildpacks** → Configure runtime environment automatically  

#### ⚙️ Buildpacks Used

- `heroku-community/apt` → Installs system dependencies  
- `heroku-buildpack-r` → Installs and configures R environment  

#### 🔄 Deployment Workflow

1. Code is pushed to the `deployment` branch  
2. Heroku detects Dockerfile / buildpacks  
3. Image is built automatically  
4. Container is deployed and exposed via assigned port  
 
---

### 🗄️ Data Persistence

- Managed via **JawsDB MySQL (Heroku add-on)**
- Externalized database ensures:
  - Data durability
  - Separation of concerns
  - Independent scalability

---

### 🔐 Configuration & Security

- Environment variables managed via `.env` (local) and Heroku config vars
- Sensitive data (credentials, tokens) is never hardcoded
- Secure database connections via pooled connections (`pool` package)

---

### 📌 Key Advantages of This Deployment Strategy

- ✅ Fully reproducible environments  
- ✅ Scalable cloud infrastructure  
- ✅ Automated testing and validation  
- ✅ Clean separation between app and data layers  
- ✅ Production-ready architecture  

---

## 📘 Full Documentation

For full system architecture, AI models, and development details, refer to the `main` branch.

