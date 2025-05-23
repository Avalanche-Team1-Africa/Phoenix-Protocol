"use client";

import React, { useState } from "react";
import { Card, CardContent, CardDescription, CardFooter, CardHeader, CardTitle } from "@/components/ui/card";
import { Button } from "@/components/ui/button";
import { Input } from "@/components/ui/input";
import { Alert } from "@/components/ui/alert";
import { AlertTriangle, Upload, Info } from "lucide-react";
import { useWallet } from "@/context/wallet-context";

export function DisputeForm() {
  const { wallet } = useWallet();
  const [transactionHash, setTransactionHash] = useState("");
  const [intentId, setIntentId] = useState("");
  const [description, setDescription] = useState("");
  const [evidenceFiles, setEvidenceFiles] = useState<File[]>([]);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [success, setSuccess] = useState(false);

  const handleFileChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    if (e.target.files) {
      const filesArray = Array.from(e.target.files);
      setEvidenceFiles([...evidenceFiles, ...filesArray]);
    }
  };

  const removeFile = (index: number) => {
    const newFiles = [...evidenceFiles];
    newFiles.splice(index, 1);
    setEvidenceFiles(newFiles);
  };

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    
    if (!wallet.connected) {
      setError("Please connect your wallet to submit a dispute");
      return;
    }
    
    if (!transactionHash || !description) {
      setError("Please fill in all required fields");
      return;
    }
    
    setLoading(true);
    setError(null);
    
    try {
      // In a real implementation, this would submit the dispute to the blockchain/API
      // Mock implementation for demo
      await new Promise(resolve => setTimeout(resolve, 2000));
      
      setSuccess(true);
      setLoading(false);
      
      // Reset form
      setTimeout(() => {
        setTransactionHash("");
        setIntentId("");
        setDescription("");
        setEvidenceFiles([]);
        setSuccess(false);
      }, 5000);
    } catch (error) {
      console.error("Error submitting dispute:", error);
      setError("Failed to submit dispute");
      setLoading(false);
    }
  };

  return (
    <Card>
      <CardHeader>
        <CardTitle>Submit Dispute</CardTitle>
        <CardDescription>
          Request DAO review for a transaction or recovery issue
        </CardDescription>
      </CardHeader>
      <CardContent>
        {error && (
          <Alert className="bg-red-500/10 border-red-500/20 text-red-500 mb-4">
            <AlertTriangle className="h-4 w-4 mr-2" />
            {error}
          </Alert>
        )}
        
        {success ? (
          <Alert className="bg-green-500/10 border-green-500/20 text-green-500 mb-4">
            <div className="flex flex-col">
              <p className="font-medium">Dispute submitted successfully!</p>
              <p className="text-sm">Your dispute has been submitted to the DAO for review. You will be notified of any updates.</p>
            </div>
          </Alert>
        ) : (
          <form onSubmit={handleSubmit} className="space-y-4">
            <Alert className="bg-blue-500/10 border-blue-500/20">
              <div className="flex items-start gap-2">
                <Info className="h-4 w-4 text-blue-500 mt-0.5" />
                <div>
                  <p className="text-sm font-medium text-blue-500">How Dispute Resolution Works</p>
                  <p className="text-xs text-muted-foreground">
                    The Dispute Resolution DAO reviews cases where transactions don't match intents or recovery requests are contested.
                    Both parties can submit evidence, and the DAO votes on the outcome based on verifiable inputs.
                  </p>
                </div>
              </div>
            </Alert>
            
            <div className="space-y-2">
              <label htmlFor="disputeType" className="text-sm font-medium">
                Dispute Type
              </label>
              <select 
                id="disputeType" 
                className="flex h-10 w-full rounded-md border border-input bg-background px-3 py-2 text-sm"
              >
                <option value="transaction">Transaction Intent Mismatch</option>
                <option value="recovery">Contested Recovery Request</option>
                <option value="guardian">Guardian Misconduct</option>
                <option value="other">Other</option>
              </select>
            </div>
            
            <div className="space-y-2">
              <label htmlFor="transactionHash" className="text-sm font-medium">
                Transaction Hash <span className="text-red-500">*</span>
              </label>
              <Input 
                id="transactionHash" 
                type="text" 
                placeholder="0x..." 
                value={transactionHash}
                onChange={(e) => setTransactionHash(e.target.value)}
                required
              />
            </div>
            
            <div className="space-y-2">
              <label htmlFor="intentId" className="text-sm font-medium">
                Intent ID or Recovery ID (if applicable)
              </label>
              <Input 
                id="intentId" 
                type="text" 
                placeholder="0x..." 
                value={intentId}
                onChange={(e) => setIntentId(e.target.value)}
              />
            </div>
            
            <div className="space-y-2">
              <label htmlFor="description" className="text-sm font-medium">
                Description <span className="text-red-500">*</span>
              </label>
              <textarea 
                id="description" 
                className="flex min-h-[120px] w-full rounded-md border border-input bg-background px-3 py-2 text-sm"
                placeholder="Describe the issue in detail..."
                value={description}
                onChange={(e) => setDescription(e.target.value)}
                required
              ></textarea>
            </div>
            
            <div className="space-y-2">
              <label className="text-sm font-medium">
                Evidence Files
              </label>
              <div className="border border-dashed border-input rounded-md p-4">
                <div className="flex flex-col items-center justify-center space-y-2">
                  <Upload className="h-8 w-8 text-muted-foreground" />
                  <p className="text-sm text-muted-foreground">
                    Drag and drop files here, or click to browse
                  </p>
                  <Input 
                    id="evidence" 
                    type="file" 
                    className="hidden"
                    onChange={handleFileChange}
                    multiple
                  />
                  <Button 
                    type="button" 
                    variant="outline" 
                    size="sm"
                    onClick={() => document.getElementById("evidence")?.click()}
                  >
                    Browse Files
                  </Button>
                </div>
              </div>
              
              {evidenceFiles.length > 0 && (
                <div className="mt-2">
                  <p className="text-sm font-medium mb-2">Selected Files:</p>
                  <div className="space-y-2">
                    {evidenceFiles.map((file, index) => (
                      <div key={index} className="flex justify-between items-center p-2 bg-muted rounded-md">
                        <span className="text-sm truncate">{file.name}</span>
                        <Button 
                          type="button" 
                          variant="ghost" 
                          size="sm"
                          onClick={() => removeFile(index)}
                        >
                          Remove
                        </Button>
                      </div>
                    ))}
                  </div>
                </div>
              )}
            </div>
            
            <div className="space-y-2">
              <label className="text-sm font-medium">
                Contact Information (optional)
              </label>
              <Input 
                type="email" 
                placeholder="Email for updates (optional)"
              />
              <p className="text-xs text-muted-foreground">
                We'll only use this to notify you about your dispute status
              </p>
            </div>
            
            <div className="pt-2">
              <Button 
                type="submit" 
                variant="gradient" 
                className="w-full"
                disabled={loading || !wallet.connected}
              >
                {loading ? (
                  <>
                    <div className="animate-spin rounded-full h-4 w-4 border-b-2 border-white mr-2"></div>
                    Submitting...
                  </>
                ) : (
                  "Submit Dispute"
                )}
              </Button>
            </div>
          </form>
        )}
      </CardContent>
    </Card>
  );
}