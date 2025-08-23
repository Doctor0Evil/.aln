```markdown
## (A) Process Sale: Workflow Handler
```aln
ACTION handle_process_sale
    INPUT user_id string
    EXEC
        LOG 🛒 User {user_id} → Launching New Sale
        # Sample workflow: scan items, input payment, confirm
        items = []
        LOG "   [Scan barcode or type item code]"
        # ... (simulate scan/input loop)
        items = ["Snickers Bar", "Water (1L)"]
        LOG "   [Enter payment method: cash, card, mobile]"
        pay_method = "card"
        LOG "   [Amount tendered?]"
        amt = 3.50
        LOG "   Processing transaction..."
        tx_result = ALN_Master_Control.execute_command("PROCESS_SALE", {
            cashier_id: user_id, items: items, payment_method: pay_method, amount_tendered: amt
        })
        LOG "   → Sale Result: {tx_result.status} (Txn ID: {tx_result.id})"
```

## (B) Inventory Management Integration
```aln
ACTION handle_inventory_management
    INPUT user_id string
    EXEC
        LOG 📦 "Inventory Management Menu"
        # List 5 most recent inventory changes
        changes = INVENTORY.get_recent_changes(limit=5)
        FOR change IN changes
            LOG f"* {change['item']}: {change['qty']} @ {change['when']}"
        ENDFOR
        LOG "Type 'restock ITEM QTY' or 'audit' for full report."
```

## (C) Compliance & Audit Handler
```aln
ACTION handle_compliance_audit
    INPUT user_id string
    EXEC
        LOG 📋 "Compliance & Audit Center"
        logs = AUDIT.get_logs(user_id=user_id, since="today")
        FOR entry IN logs
            LOG f"* {entry['event']} ({entry['time']})"
        ENDFOR
        LOG "Type 'export logs' to email CSV."
```
