```tsx
import React, { useState } from "react";

export function OnboardingWizard() {
  const [step, setStep] = useState(1);
  return (
    <div className="onboarding-wizard">
      {step === 1 && (
        <section>
          <h2>Welcome to the IOM!</h2>
          <p>
            Discover and help recover lost music, videos, and media with the world’s first AI-powered Community Memory Bridge.
          </p>
          <button onClick={() => setStep(2)}>Next</button>
        </section>
      )}
      {step === 2 && (
        <section>
          <h3>How it works</h3>
          <ol>
            <li>Search or describe a lost song or video, as best you remember.</li>
            <li>Let our AI and the global community hunt for matches.</li>
            <li>Submit, confirm, and celebrate solved mysteries!</li>
          </ol>
          <button onClick={() => setStep(3)}>Join the Hunt</button>
        </section>
      )}
      {step === 3 && (
        <section>
          <b>You’re ready!</b>
          <p>
            Try a search, or submit a new memory.<br />
            <em>Comet & Discord users get secret tools—watch for “magic banners.”</em>
          </p>
          <button onClick={() => setStep(1)}>Restart</button>
        </section>
      )}
    </div>
  );
}
```
