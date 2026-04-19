# 🏥 OdontoGest: Intelligent Clinical Management System with CDSS and RAG-based AI

[![R Shiny](https://img.shields.io/badge/Framework-R%20Shiny-blue.svg)](https://shiny.posit.co/)
[![Salesforce](https://img.shields.io/badge/AI-Salesforce%20Agentforce-blueviolet.svg)](https://www.salesforce.com/products/einstein/agentforce/)
[![Status](https://img.shields.io/badge/Status-Final%20Degree%20Project-orange.svg)]()
[![Docker](https://img.shields.io/badge/Container-Docker-blue.svg)]()

**OdontoGest** is a comprehensive clinical management platform for dental clinics that transforms the traditional reactive management approach into a **proactive, data-driven paradigm**. It integrates a robust web architecture with a **Clinical Decision Support System (CDSS)** powered by Machine Learning and an AI assistant utilizing **Retrieval-Augmented Generation (RAG)**.

---

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

![Navigation Diagram](./docs/Diagrama_navegacion_OdontoGest.png)
