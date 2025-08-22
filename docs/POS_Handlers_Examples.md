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
