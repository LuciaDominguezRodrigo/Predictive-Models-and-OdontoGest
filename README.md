# 🏥 OdontoGest: Intelligent Clinical Management System with CDSS and RAG-based AI

[![R Shiny](https://img.shields.io/badge/Framework-R%20Shiny-blue.svg)](https://shiny.posit.co/)
[![Salesforce](https://img.shields.io/badge/AI-Salesforce%20Agentforce-blueviolet.svg)](https://www.salesforce.com/products/einstein/agentforce/)
[![Status](https://img.shields.io/badge/Status-Final%20Degree%20Project-orange.svg)]()
[![Docker](https://img.shields.io/badge/Container-Docker-blue.svg)]()

**OdontoGest** is a comprehensive clinical management platform for dental clinics that transforms the traditional reactive management approach into a **proactive, data-driven paradigm**. It integrates a robust web architecture with a **Clinical Decision Support System (CDSS)** powered by Machine Learning and an AI assistant utilizing **Retrieval-Augmented Generation (RAG)**.

---
## 📂 Project Structure & Branching Strategy

The repository is organized into two main branches to separate the development environment from the production-ready deployment.

---

## 🌿 Branching Strategy

### `main` branch
Contains the full development environment, including:

- Local development tools
- Raw datasets
- Model training scripts
- Full documentation

### `deployment` branch
Contains a streamlined, production-ready version optimized for **Heroku deployment**.  
This branch includes:

- `ClinicAppTFG` core application
- Docker configuration
- Production-specific environment settings
- Lightweight dependencies required for execution

---

## 📁 Directory Structure (main branch)

```plaintext
├── .github/workflows/       # CI/CD pipelines (GitHub Actions)
├── ClinicAppTFG/            # 📦 Core Shiny Application
│   ├── modules/             # UI/Server logic decoupled by functionality
│   ├── tests/               # Unit and integration tests
│   ├── www/                 # Static assets (CSS, Images, JS)
│   ├── app.R                # Application entry point
│   ├── global.R             # Global variables and library loading
│   └── db_init.R            # Database connection and pooling logic
├── modelos/                 # 🧠 Machine Learning Research
│   ├── modeloDiagnostico/   # XGBoost training scripts and saved weights
│   ├── modeloStock/         # Linear Regression scripts for inventory
│   └── comparacion/         # Model evaluation metrics and SHAP analysis
├── docs_readme/             # Documentation assets and diagrams
├── .gitignore               # Files excluded from version control
└── README.md                # Project documentation

```

## 🧩 Module Architecture

| Module | Description |
|--------|-------------|
| `appointments` | Scheduling system |
| `clinic_history` | Patient clinics records management |
| `diagnosis` | CDSS interface (XGBoost-based) |
| `stock_management` | Inventory forecasting system |
| `user_management` | User creationn, deletion and ban/unban functions |
| `medical_certificate` | Medical certificate generation as a PDF for patients |
| `contact_management` | Management of the contact form and the response to it |
| `index` | Main page, function as orchestrator |
| `lab` | Comercial and labs functionality |
| `login` | login/logout functionality |
| `profile` | Manage profile info and changes on it|
| `public` | Landing page|
| `reset_password` | Manage reset password logic|




---

## 💡 Design Principles

### 🔒 Isolation
Training models and datasets are separated from the production codebase in order to reduce application weight, improve security, and ensure a clean deployment environment.

---

### ⚙️ Scalability
Each feature is implemented as an independent module, preventing monolithic architecture and allowing the system to scale and evolve without structural complexity.

---

### 🚀 Deployment Ready
The `deployment` branch represents a production-optimized build of the system, specifically configured for deployment on **Heroku** with only the necessary runtime components.



##  Key Value Propositions

### 🏗️ Advanced Software Engineering
* **SPA Architecture (Single Page Application):** Implemented using dynamic navigation, History API integration, and `sessionStorage` to maintain state across the R Shiny environment without page reloads.
* **Modular Design:** Strictly decoupled architecture using *Namespacing*, ensuring high maintainability, testability, and scalability.
* **Granular RBAC:** Role-Based Access Control managing 8 distinct profiles (Doctor, Hygienist, Patient, Admin, Receptionist, Laboratory, Commercial, and Visitor).

### 🧠 Intelligence & Decision Support (CDSS)
* **Inventory Forecasting:** A log-linear regression model (`caret`) that predicts monthly supply demands to prevent stockouts.
* **Diagnostic Assistance:** An **XGBoost** classifier trained to assist in periodontal disease severity assessment.
* **Explainable AI (XAI):** Integration of **SHAP values** to provide clinical transparency, allowing doctors to understand the "why" behind every AI suggestion.

### 🤖 Generative AI: Agentforce & RAG
The system features **"Dientecito"**, an intelligent agent orchestrated via the Salesforce ecosystem:
* **RAG Pipeline:** Extracts semantic knowledge from clinical guidelines and nutritional protocols (PDFs) to generate grounded, hallucination-free responses.
* **Atlas Reasoning Engine:** Interprets user intent and dynamically selects the correct knowledge topics.
* **Einstein Trust Layer:** Ensures clinical data privacy by anonymizing sensitive information before processing.

---

## 🛠️ Technology Stack

| Layer | Technologies |
| :--- | :--- |
| **Backend** | R 4.x, Shiny, Pool Connection (MySQL/PostgreSQL) |
| **Data Science** | `caret`, `xgboost`, `fastshap`, `ggplot2` |
| **Security** | BCrypt (Hashing), Sodium (Tokens), Secure Cookies |
| **Integration** | Salesforce Agentforce, Data Cloud, Tampermonkey (Secure Bridge) |
| **DevOps** | Docker, Heroku, GitHub Actions (CI/CD) |

---

## 🔄 RAG Architecture & Security Bridge

A unique feature of this project is the **Secure AI Bridge** implemented via **Tampermonkey**:
1. **Isolation:** The AI agent logic is decoupled from the main Shiny source code to protect Salesforce API credentials.
2. **Zero Trust Tunnel:** Interaction with the **Atlas Reasoning Engine** is only activated on authorized workstations with the control script injected.
3. **Grounding:** Every response from "Dientecito" is cross-referenced with the internal clinical library, ensuring the advice follows official medical protocols.


## 👥System Roles

### 👤 Visitor
Unauthenticated user.
- Access to the home page.
- Contact form access.


### 🧑‍⚕️ Patient
Restricted access to their own information.
- Manage appointments.
- Update profile.
- Download medical assistance certificates.
- For security reasons, if a patient wishes to obtain a copy of their clinical history, it must be provided by authorized clinical staff.


### 🏥 Receptionist
Operational core of the system.
- Manage the global schedule
- Handle the inbox from the contact form
- Create/Delete user accounts 
- ❌ No access to sensitive clinical data (e.g., diagnoses)


### 👨‍⚕️ Clinical Staff (Doctor / Hygienist)
Users with access to clinical systems.
- Use the Clinical Decision Support System (CDSS) that includes  Machine Learning models for:
  - Diagnostics
  - Inventory management
- Visualize, edit and download Patients Clinical History (understanding editing as addding new clinical notes)
- Visualize order status (both protesic and stock orders). Also update the order status (in the case that the order must be returned, indicate why).
- Visualize the calendar, with the data for each appointment. If the appointment has already occurs, add clinical relevant notes.
- Update profile info.


### 🛠️ Administrator (Admin)
Superuser with full system access.
- Full system auditing
- Manage user status (Ban / Unban)
- Access to all modules


### 🧪 Commercial / Laboratory (Lab)
External role with limited access.
- Access the order management module
- Update manufacturing status
- Provide tracking information
- ❌ No access to clinical or personal data

---


## 🧭 Application Navigation Overview

This diagram provides a **high-level overview** of the application's navigation structure.

It is intended to give a general understanding of how users move through the system and how the main modules are organized.

### 📝 Notes
- This is a **simplified representation** of the system.
- Some screens are **shared across multiple user roles**, but are not duplicated in the diagram for clarity.
- These shared views and detailed flows are fully described in the **User Manual**.

### 🎨 Legend (Color Coding)

The diagram uses color coding to represent access by different user roles:

- 🔵 **Light Blue**: General screens (shared/common)
- ⚪ **Grey**: External screens (without login)
- 🟢 **Light Green**: Patient
- 🟣 **Purple**: Commercial / Laboratory
- 🔷 **Dark Blue**: Administrator
- 🌸 **Pink**: Receptionist
- 🟡 **Yellow**: Clinical Staff

### 🗺️ Navigation Diagram

![Navigation Diagram](./docs_readme/navigation_map.png)


## 🤖 Predictive Models Overview

This system integrates two Machine Learning models designed to support both **clinical decision-making** and **operational management** within the clinic.

The architecture follows a **problem-specific modeling approach**, where each model is selected based on the nature of the task, prioritizing **robustness, interpretability, and reliability**.

---

### 🧠 Model 1: Stock Demand Prediction

#### 📌 Objective
Estimate future material demand in order to optimize inventory management and avoid:
- Overstock (resource waste)
- Stock shortages (treatment delays)

#### ⚙️ Selected Model
- **Linear Regression**

#### ✅ Justification
Although more complex models (Random Forest, XGBoost) were evaluated, Linear Regression was selected due to:

- Better generalization
- Robustness to noise
- Stability in predictions
- High interpretability
- Low computational cost

---

### 🏥 Model 2: Clinical Diagnosis Support

#### 📌 Objective
Support clinical staff in the preliminary classification of patients.

> ⚠️ **Adaptation for application integration**  
> Although the model was originally developed to classify patients into four severity levels  
> (**Normal, Mild, Moderate, Severe**), for integration into the application it has been  
> **adapted to clinically meaningful categories**:
>
> - **Healthy (Normal)**
> - **Caries**
> - **Periodontitis**

This mapping allows the model outputs to be directly aligned with real clinical workflows and usability within the system.

#### ⚙️ Selected Model
- **XGBoost (Gradient Boosting)**

#### ✅ Justification
- High predictive performance
- Strong sensitivity in critical cases
- High specificity in healthy patients
- Ability to model complex, non-linear relationships
- Robustness under cross-validation

#### ⚠️ Clinical Considerations
- The model is designed to **assist**, not replace, clinical judgment
- Special care is taken to minimize **false negatives in severe conditions**

---

### 🔍 Model Interpretability

- SHAP (SHapley Additive Explanations) is used to explain predictions
- Key variables such as **age** and **glucose levels** drive model decisions
- Each prediction can be broken down into feature contributions

---

### 🔒 Usage and Access

- Accessible only by **Clinical Staff**
- Integrated within the **Clinical Decision Support System (CDSS)**
- Outputs are intended as **decision support**

---

### ⚠️ Limitations

- Dependent on data quality
- Higher uncertainty in intermediate cases
- Requires continuous validation

---

### 🔄 Future Improvements

- Integration of real clinical datasets
- Improved classification granularity
- Inclusion of additional clinical variables

## ☁️ AI Assistant Agent: Salesforce Agentforce Integration (via Tampermonkey)

The application features an intelligent assistant powered by Salesforce Agentforce. To seamlessly integrate the Salesforce chat capabilities into the web frontend without modifying the core production build during the testing phase, a Tampermonkey script approach was utilized.

---

### ⚙️ Integration Workflow

#### 🧩 Script Injection
A custom userscript (Tampermonkey) is used to inject the Salesforce Embedded Messaging Bootstrap directly into the browser session.

#### 🔐 Initialization
The script handles the following configuration values to establish a secure connection with Salesforce Service Cloud:

- `ORG_ID`
- `DEPLOYMENT_NAME`
- `SITE_URL`

---

### 🚀 Key Capabilities

- 💬 Real-time queries regarding clinic treatments specifications
- 🔄 Personaliced dental routines

---

## 🚀 Deployment on Heroku and Buildpacks

The platform is deployed using **Heroku**, a Platform-as-a-Service (PaaS) that enables seamless application deployment, scalability, and maintenance in the cloud without requiring direct infrastructure management.

### ⚙️ Deployment Strategy

The application follows a container-based deployment approach combined with buildpacks, which automate the environment preparation before the application is executed.

This strategy ensures:

- 🔄 **Reproducible deployments** across different environments (development, testing, and production).
- 📦 **Automated management of system and R dependencies**.
- ⚙️ **Horizontal scalability** handled natively by Heroku.

---

### 🧱 Buildpacks Used

To properly configure the execution environment, the following buildpacks are used:

- `heroku-community/apt`  
  Enables the installation of low-level system dependencies required by the application.

- `heroku-buildpack-r`  
  Handles the installation, compilation, and optimization of the R runtime environment in the cloud.

---

### 🗄️ Data Persistence

Data persistence is managed using the **JawsDB MySQL** add-on, which provides a fully managed external relational database service.

This approach allows:

- 🔒 Separation between application logic (Shiny server) and data storage.
- 📊 Secure storage of sensitive clinical data, such as patient records.
- ☁️ Independent scalability of the database layer.

---

### 📌 Architectural Considerations

Thanks to this deployment strategy:

- The application can scale dynamically based on user demand.
- Production environment maintenance is significantly simplified.
- High system availability is ensured in the cloud.

---

💡 Overall, the combination of Heroku, buildpacks, and an external database service results in a robust, modular architecture suitable for real-world clinical environments.
