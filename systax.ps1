# Contract Compliance Shell — Clause-by-Clause Setup Simulation

# 1. Set working directory per policy
Set-Location -Path 'C:\Users\Hunter\private'

# 2. Define clauses, policies, and compliance scope in hashtable structure
$Contract = @{
    party                           = 'The entity entering into this contract'
    effective_date                  = 'Date contract starts'
    recitals                        = 'Why parties are making this agreement'
    definitions                     = 'All key terms clarified'
    services_provided               = "Exact description of what's delivered"
    payment_terms                   = 'How and when payment occurs'
    confidentiality_clause          = 'No disclosure of secret info'
    intellectual_property_clause    = @(
        'All work produced is owned by the company',
        'employee hereafter assigns all rights to the company',
        'works made in scope of agreement are "works for hire"',
        'employee must disclose inventions immediately',
        'employee executes documents to perfect rights'
    )
    warranty_clause                 = @{
        guarantees_no_infringe      = 'Provider guarantees no infringement of third-party IP'
        disclaimer_of_implied_warranty = 'Limits provider''s liability'
        knowledge_qualified_warranty = 'Only covers known infringement'
    }
    indemnity_clause                = @{
        provider_defends            = 'Provider defends against IP claims'
        provider_pays               = 'Provider pays damages, costs, settlements'
        scope_defined_as            = 'trade secret, patent, copyright, trademark'
        trigger_events              = 'third-party claim filed'
        limit_indemnity             = 'No duty where customer misuses product'
        max_liability_cap           = 'Dollar value'
        notice_and_cooperation      = 'Customer promptly notifies'
    }
    license_rights                  = @(
        'research_actual_owner',
        'negotiate_license_scope',
        'sign_and_record_license',
        'monitor_ongoing_compliance'
    )
    employee_obligations            = @(
        'must_promptly_disclose_inventions',
        'must_assign_all_rights_to_company',
        'must_sign_docs_to_effect_assignment',
        'must_warrant_no_conflict_with_other_agreements',
        'must_not_use_third_party_ip_unless_authorized'
    )
    termination_clause              = @(
        'How parties may end agreement',
        'Notice period required',
        'Post-termination survival clauses'
    )
    dispute_resolution              = @(
        'mediation_before_litigation',
        'jurisdiction_defined',
        'attorney_fees_awarded_to_winner'
    )
    amendment_procedure             = 'How changes are valid'
    force_majeure                   = 'No liability for acts outside control'
    signatures                      = 'Both parties sign and date below'
    jurisdiction                    = 'Courts of a named location apply'
    remedies_for_breach             = @{
        damages                     = 'Money for loss'
        specific_performance        = 'Court forces action'
        contract_termination        = 'Ends agreement'
    }
    assignment_of_agreement         = 'Can transfer or not'
    notice_methods                  = 'Email, physical mail, etc.'
    governing_law                   = 'Which state/country law applies'
    attachments_and_exhibits        = 'Additional details'
    boilerplate_clauses             = @{
        entire_agreement            = 'Supersedes all prior deals'
        severability                = 'If one part fails, others stay'
        headings                    = "Titles don't control agreement"
    }
    compliance_statement            = @(
        'Each party follows all relevant laws',
        'immediate notification of non-compliance'
    )
    audit_rights                    = 'Company may inspect compliance'
    exclusivity_clause              = 'Supplier provides only to company'
    non_compete_clause              = 'Cannot work for competitors X years'
    IP_monitoring                   = @{
        automated_searches          = 'Track logos/text online'
        google_alerts               = 'Set keywords and monitor'
        social_profile_scanning     = 'Check Instagram, TikTok, LinkedIn'
        takedown_requests           = 'Issue DMCA/cease & desist'
    }
    data_protection                 = @{
        personal_data_handling      = 'all personal data handled as per law'
        privacy_policy_linked       = $true
    }
    trade_secret_provision          = @{
        secure_storage_required     = $true
        limit_disclosure            = 'need to know'
        immediate_remedy_for_breach = $true
    }
    contract_compliance_check       = 'Random audits scheduled'
}

# 3. Simulate random contract compliance check function
Function Invoke-ContractComplianceCheck {
    Param($ContractObj)
    $clauses = $ContractObj.Keys
    $sampleClause = Get-Random -InputObject $clauses
    Write-Host "Conducting compliance audit for clause: $sampleClause"
    if ($ContractObj[$sampleClause]) {
        Write-Host "Clause [$sampleClause]: Present and defined."
    } else {
        Write-Host "Clause [$sampleClause]: MISSING! Potential compliance GAP."
    }
}

# 4. Display summary/status of contract structure
Write-Host "---- CONTRACT STRUCTURE [Perpetual Audit Ready] ----"
$Contract.Keys | ForEach-Object { Write-Host "$_" }
Write-Host "`nRandom clause compliance check simulation:"
Invoke-ContractComplianceCheck -ContractObj $Contract

# 5. Simulated outcomes
# - All keys defined: "Clause [X]: Present and defined."
# - If a key is missing: "Clause [X]: MISSING! Potential compliance GAP."
